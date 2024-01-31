library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)
library(readr)

html <- read_html("Group-based/Group-based screen on title & abstract full coding report.html")

screen_report_dat_raw <- 
  html |> 
  html_element("table") |> 
  html_table() |> 
  filter(str_detect(`I/E/D/S flag`, "(I)|(E)"))

screen_report_dat_raw |> glimpse()

screen_report_dat <- 
  screen_report_dat_raw |> 
  select(-`I/E/D/S flag`) |> 
  rename(
    eppi_id= ItemId,
    author_short = ShortTitle, 
    title_report = Title, 
    exclude1 = `EXCLUDE review` ,
    exclude2 = EXCLUDE,
    include = `INCLUDE on title & abstract`
  ) |> 
  mutate(
    across(exclude1:include, ~ str_replace_all(.x, "Rune KlitgÃ¥rd", "Rune Klitgård"))
  ) |> 
  arrange(eppi_id) 

# Loading included and excluded studies
ex_paths <- list.files("Group-based/", pattern = "excl")

grp_based_excl <- 
  map(ex_paths, ~ {
    revtools::read_bibliography(paste0("Group-based/", .x)) |> 
      suppressWarnings() |> 
      as_tibble() |>
      select(author, eppi_id, title, abstract) |> # Using only relevant variables
      mutate(
        final_human_decision = 0
      )
  }
  ) |> 
  list_rbind()


grp_based_incl <- 
  revtools::read_bibliography("Group-based/group_based_incl.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 1
  )

grp_based_ris_dat <- bind_rows(filter(grp_based_excl, eppi_id != "73490249"), grp_based_incl) 
#rm(grp_based_excl, grp_based_incl)

grp_based_dat_wide <- 
  left_join(screen_report_dat, grp_based_ris_dat, by = join_by(eppi_id)) |> 
  arrange(final_human_decision)

# Detecting individual screener names
screeners_var <- 
  screen_report_dat |> 
  reframe(
    name = unique(c_across(exclude1:include))
  ) |> 
  pull(name)

screeners <- 
  unlist(strsplit(screeners_var, "(?<=[a-z])(?=[A-Z])", perl = TRUE)) |> 
  gsub("\\[.*","", x = _) |> 
  unique() 

screeners <- screeners[1:8]

filter_list <- list()

for (i in 1:length(screeners)){
  filter_list[[i]] <- 
    grp_based_dat_wide |> 
    filter(if_any(exclude1:include, ~ str_detect(.x, screeners[i]))) |> 
    mutate(
      screener = screeners[i],
      across(exclude1:include, ~ str_extract(.x, screeners[i]))
    ) |> 
    relocate(screener)
}

single_screen_dat <- 
  filter_list |> 
  list_rbind() |> 
  mutate(
    #exclude = if_else(!is.na(exclude), screener, NA_character_),
    exclude = if_any(exclude1:exclude2, ~ !is.na(.x)),
    exclude = if_else(exclude == TRUE, screener, NA_character_),
    screener_decision = case_when(
      !is.na(include) ~ 1,
      !is.na(exclude) ~ 0,
      !is.na(include) & !is.na(exclude) ~ 2, # Indicating non decisive answer
      TRUE ~ NA_real_
    ),
    conflict = if_else(screener_decision != final_human_decision, 1, 0)
  ) |> 
  select(-c(exclude1:exclude2)) |> 
  relocate(exclude, .before = include) |> 
  relocate(screener_decision, .before = final_human_decision)

# Check
sum(single_screen_dat$screener_decision == 2)

# All screenings including training sessions
grp_based_single_perform_dat <- 
  single_screen_dat |> 
  summarise(
    TP = sum(screener_decision == 1 & final_human_decision == 1),
    TN = sum(screener_decision == 0 & final_human_decision == 0),
    FN = sum(screener_decision == 0 & final_human_decision == 1),
    FP = sum(screener_decision == 1 & final_human_decision == 0),
    recall = TP / (TP + FN),
    spec = TN / (TN + FP),
    bacc = (recall + spec) / 2,
    .by = screener
  ) |> 
  mutate(
    review_authors = "Dalgaard, Jensen, et al. (2022)",
    review = "Group-based",
    role = rep(c("Author", "Assistant", "Author"), c(1, 5, 2))
  ) |> 
  relocate(review_authors:role) 

saveRDS(grp_based_single_perform_dat, "single screener data/All screenings/grp_based_single_perform_dat.rds")

#----------------------------------------------------------------------------------------
# Extracting all individual screener scores in wide format to exclude training references
#----------------------------------------------------------------------------------------

single_screen_dat_wide <- 
  single_screen_dat |> 
  pivot_wider(
    id_cols = eppi_id,
    id_expand = TRUE,
    values_from = screener_decision,
    names_from = screener,
  )

grp_based_dat <- 
  left_join(grp_based_dat_wide, single_screen_dat_wide, by = join_by(eppi_id)) |> 
  relocate(final_human_decision, .after = last_col()) |> 
  rowwise() |> 
  mutate(
    n_screeners = sum(!is.na(c_across(`Nina Thorup Dalgaard`:`Mikkel  Vembye`)))
  ) |> 
  ungroup()

n_refs <- grp_based_dat |> filter(n_screeners == 2) |> nrow()
saveRDS(n_refs, "single screener data/Number of References/grp_based_n_refs.rds")

n_in <- grp_based_dat |> filter(n_screeners == 2 & final_human_decision == 1) |> nrow()
saveRDS(n_in, "single screener data/Number of References/grp_based_n_in.rds")

grp_based_dat_2screen <- 
  grp_based_dat |> 
  filter(n_screeners == 2) |>
  select(-n_screeners) |> 
  pivot_longer(
    cols = `Nina Thorup Dalgaard`:`Mikkel  Vembye`,
    names_to = "screener",
    values_to = "screener_decision"
  ) |> 
  filter(!is.na(screener_decision)) |> 
  arrange(screener, final_human_decision) |>
  relocate(screener:screener_decision, .before = final_human_decision)

grp_based_single_perform_dat_2screen <- 
  grp_based_dat_2screen |> 
  summarise(
    TP = sum(screener_decision == 1 & final_human_decision == 1, na.rm = TRUE),
    TN = sum(screener_decision == 0 & final_human_decision == 0, na.rm = TRUE),
    FN = sum(screener_decision == 0 & final_human_decision == 1, na.rm = TRUE),
    FP = sum(screener_decision == 1 & final_human_decision == 0, na.rm = TRUE),
    recall = TP / (TP + FN),
    spec = TN / (TN + FP),
    bacc = (recall + spec) / 2,
    .by = screener
  ) |> 
  ungroup() |> 
  mutate(
    review_authors = "Dalgaard, Jensen, et al. (2022)",
    review = "Group-based",
    role = rep(c("Assistant", "Author", "Assistant"), c(3, 3, 1))
  ) |> 
  relocate(review_authors:role)

saveRDS(grp_based_single_perform_dat_2screen, "single screener data/grp_based_single_perform_dat_2screen.rds")
