library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)

html <- read_html("inclusion/Inclusion screen on title and abstract full coding report.html")

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
    exclude1 = `EXCLUDE`, 
    exclude2 = `EXCLUDE review`,
    include = `INCLUDE on title & abstract`
  ) |> 
  arrange(eppi_id)

rm(screen_report_dat_raw)

# Loading included and excluded studies
ex_paths <- list.files("inclusion/", pattern = "excl")

inclusion_excl <- 
  map(ex_paths, ~ {
    revtools::read_bibliography(paste0("inclusion/", .x)) |> 
      suppressWarnings() |> 
      as_tibble() |>
      select(author, eppi_id, title, abstract) |> # Using only relevant variables
      mutate(
        final_human_decision = 0
      )
  }
  ) |> 
  list_rbind()

inclusion_incl <- revtools::read_bibliography("inclusion/includeTA.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |>
  mutate(
    final_human_decision = 1
  )

inclusion_ris_dat <- bind_rows(inclusion_excl, inclusion_incl) 
ids <- inclusion_ris_dat |> pull(eppi_id)

rm(inclusion_incl)
rm(inclusion_excl)

# Excluded reference
excluded_ref <- 
  screen_report_dat |> 
  filter(!eppi_id %in% ids)

screen_report_dat_filtered <- 
  screen_report_dat |> 
  filter(eppi_id %in% ids) |> 
  mutate(
    across(exclude1:include, ~ str_replace_all(.x, "Tanne Ebert JÃ¸rgensen", "Tanne Ebert Jørgensen")),
    across(exclude1:include, ~ str_replace_all(.x, "Malene  Wallach Kildemoes", "Malene Wallach Kildemoes"))
  )

rm(screen_report_dat)
rm(ids)

inclusion_dat_wide <- 
  left_join(screen_report_dat_filtered, inclusion_ris_dat, by = join_by(eppi_id)) |> 
  arrange(final_human_decision) 

rm(inclusion_ris_dat)

# Detecting individual screener names
screeners_var <- 
  screen_report_dat_filtered |> 
  reframe(
    name = unique(c_across(exclude1:include))
  ) |> 
  pull(name)

screeners <- 
  unlist(strsplit(screeners_var, "(?<=[a-z])(?=[A-Z])", perl = TRUE)) |> 
  gsub("\\[.*","", x = _) |> 
  unique()

filter_list <- list()

for (i in 1:length(screeners)){
  filter_list[[i]] <- 
    inclusion_dat_wide |> 
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
    exclude = if_any(exclude1:include, ~ !is.na(.x)),
    exclude = if_else(exclude == TRUE, screener, NA_character_),
    screener_decision = case_when(
      !is.na(include) ~ 1,
      !is.na(exclude) ~ 0,
      !is.na(include) & !is.na(exclude) ~ 2, # Indicating non decisive answer
      TRUE ~ NA_real_
    ),
    conflict = if_else(screener_decision != final_human_decision, 1, 0)
  ) |> 
  relocate(exclude, .before = include) |> 
  relocate(screener_decision, .before = final_human_decision)

# All screenings including training sessions
inclusion_single_perform_dat <- 
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
    review_authors = "Dalgaard, Bondebjerg, Viinholt, et al. (2022)",
    review = "Inclusion",
    role = rep(c("Assistant", "Author", "Assistant"), c(3,2,2))
  ) |> 
  relocate(review_authors:role) 

saveRDS(inclusion_single_perform_dat, "single screener data/All screenings/inclusion_single_perform_dat.rds")


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

inclusion_dat <- 
  left_join(inclusion_dat_wide, single_screen_dat_wide, by = join_by(eppi_id)) |> 
  relocate(final_human_decision, .after = last_col()) |> 
  rowwise() |> 
  mutate(
    n_screeners = sum(!is.na(c_across(`Kristine Grosen Ellermann`:`Tanne Ebert Jørgensen`)))
  ) |> 
  ungroup()

n_refs <- inclusion_dat |> filter(n_screeners == 2) |> nrow()
saveRDS(n_refs, "single screener data/Number of References/inclusion_n_refs.rds")

inclusion_dat_2screen <- 
  inclusion_dat |> 
  filter(n_screeners == 2) |>
  select(-n_screeners) |> 
  pivot_longer(
    cols = `Kristine Grosen Ellermann`:`Tanne Ebert Jørgensen`,
    names_to = "screener",
    values_to = "screener_decision"
  ) |> 
  filter(!is.na(screener_decision)) |> 
  arrange(screener, final_human_decision) |>
  relocate(screener:screener_decision, .before = final_human_decision)

inclusion_single_perform_dat_2screen <- 
  inclusion_dat_2screen |> 
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
    review_authors = "Dalgaard, Bondebjerg, Viinholt, et al. (2022)",
    review = "Inclusion",
    role = rep(c("Author", "Assistant", "Author", "Assistant"), c(1,4,1,1))
  ) |> 
  relocate(review_authors:role)

saveRDS(inclusion_single_perform_dat_2screen, "single screener data/inclusion_single_perform_dat_2screen.rds")
