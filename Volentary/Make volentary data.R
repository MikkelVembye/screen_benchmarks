library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)

html <- read_html("Volentary/Volentary screening on title and abstract full coding report.html")

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
    include = Include, 
    uncertain = Uncertain,
    exclude = Exclude,
    duplicate = Duplicate
  ) |> 
  arrange(eppi_id) |> 
  select(-c(uncertain, duplicate))

# Loading included and excluded studies
ex_paths <- list.files("Volentary/", pattern = "excl")

volentary_excl <- 
  map(ex_paths, ~ {
    revtools::read_bibliography(paste0("Volentary/", .x)) |> 
      suppressWarnings() |> 
      as_tibble() |>
      select(author, eppi_id, title, abstract) |> # Using only relevant variables
      mutate(
        final_human_decision = 0
      )
  }
  ) |> 
  list_rbind()


volentary_incl <- 
  revtools::read_bibliography("Volentary/includeTA.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 1
  )

volentary_ris_dat <- bind_rows(volentary_excl, volentary_incl) 
# Removing untertain and duplicate references from screening report
ids <- volentary_ris_dat$eppi_id

screen_report_dat_filtered <- 
  screen_report_dat |> 
  filter(eppi_id %in% ids)


volentary_dat_wide <- 
  left_join(screen_report_dat_filtered, volentary_ris_dat, by = join_by(eppi_id)) |> 
  arrange(final_human_decision)

rm(volentary_excl, volentary_incl)

# Detecting individual screener names
screeners_var <- 
  screen_report_dat_filtered |> 
  reframe(
    name = unique(c_across(include:exclude))
  ) |> 
  pull(name)

screeners <- 
  unlist(strsplit(screeners_var, "(?<=[a-z])(?=[A-Z])", perl = TRUE)) |> 
  gsub("\\[.*","", x = _) |> 
  unique() 


filter_list <- list()

for (i in 1:length(screeners)){
  filter_list[[i]] <- 
    volentary_dat_wide |> 
    filter(if_any(include:exclude, ~ str_detect(.x, screeners[i]))) |> 
    mutate(
      screener = screeners[i],
      across(include:exclude, ~ str_extract(.x, screeners[i]))
    ) |> 
    relocate(screener)
}

single_screen_dat <- 
  filter_list |> 
  list_rbind() |> 
  mutate(
    exclude = if_any(exclude1:exclude3, ~ !is.na(.x)),
    exclude = if_else(exclude == TRUE, screener, NA_character_),
    screener_decision = case_when(
      !is.na(include) ~ 1,
      !is.na(exclude) ~ 0,
      !is.na(include) & !is.na(exclude) ~ 2, # Indicating non decisive answer
      TRUE ~ NA_real_
    ),
    conflict = if_else(screener_decision != final_human_decision, 1, 0)
  ) |> 
  select(-c(exclude1:exclude3)) |> 
  relocate(exclude, .before = include) |> 
  relocate(screener_decision, .before = final_human_decision)

# Check
sum(single_screen_dat$screener_decision == 2)

# All screenings including training sessions
volentary_single_perform_dat <- 
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
    review_authors = "Filges, Siren, et al. (2020)",
    review = "Volentary Work",
    role = rep(c("Assistant", "Author"), c(2,2))
  ) |> 
  relocate(review_authors:role) 

saveRDS(volentary_single_perform_dat, "single screener data/All screenings/volentary_single_perform_dat.rds")

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

volentary_dat <- 
  left_join(volentary_dat_wide, single_screen_dat_wide, by = join_by(eppi_id)) |> 
  relocate(final_human_decision, .after = last_col()) |> 
  rowwise() |> 
  mutate(
    n_screeners = sum(!is.na(c_across(`Caroline Fromberg Kiehn`:`Julie Kaas Seerup`)))
  ) |> 
  ungroup()

n_refs <- volentary_dat |> filter(n_screeners == 2) |> nrow()
saveRDS(n_refs, "single screener data/Number of References/volentary_n_refs.rds")

volentary_dat_2screen <- 
  volentary_dat |> 
  filter(n_screeners == 2) |>
  select(-n_screeners) |> 
  pivot_longer(
    cols = `Caroline Fromberg Kiehn`:`Julie Kaas Seerup`,
    names_to = "screener",
    values_to = "screener_decision"
  ) |> 
  filter(!is.na(screener_decision)) |> 
  arrange(screener, final_human_decision) |>
  relocate(screener:screener_decision, .before = final_human_decision)

volentary_single_perform_dat_2screen <- 
  volentary_dat_2screen |> 
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
    review_authors = "Filges, Siren, et al. (2020)",
    review = "Volentary Work",
    role = rep(c("Assistant", "Author"), c(2,1))
  ) |> 
  relocate(review_authors:role) |> 
  filter(screener != "Trine Filges") # Only contain four references

saveRDS(volentary_single_perform_dat_2screen, "single screener data/volentary_single_perform_dat_2screen.rds")


