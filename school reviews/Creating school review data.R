# Collecting data School review (Dietrichson 2018, 2020, 2021)

library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)

html <- read_html("school reviews/School reviews on title and abstract full coding report(1).html")

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
    eppi_id = ItemId,
    author_short = ShortTitle, 
    title_report = Title, 
    include = Include,
    uncertain = Uncertain, 
    exclude1 = `Exclude: Duplicates`,
    exclude2 = `Exclude: Wrong type of intervention`,
    exclude3 = `Exclude: Wrong participants`,
    exclude4 = `Exclude: Wrong setting/country`,
    exclude5 = `Exclude: Wrong outcomes/years or review`,
    exclude6 = `Exclude: Wrong target group`,
    exclude7 = `Exclude: No standardized reading/math test`,
    exclude8 = `Exclude: Wrong study design`,
    exclude9 = `Exclude: Wrong language`
  ) |> 
  select(!`K-6`:`No info yet`) |> 
  mutate(
    across(include:exclude9, ~ str_replace_all(.x, "JÃ¸rgensen", "Jørgensen")),
    across(include:exclude9, ~ str_replace_all(.x, "BjÃ¸rn", "Bjørn")),
    across(include:exclude9, ~ str_replace_all(.x, "amanda weber", "Amanda Weber"))
  ) |> 
  arrange(eppi_id) 

# Loading included and excluded studies
ex_paths <- list.files("school reviews/", pattern = "excl")

sch_rev_excl <- 
  map(ex_paths, ~ {
    revtools::read_bibliography(paste0("school reviews/", .x)) |> 
      suppressWarnings() |> 
      as_tibble() |>
      select(author, eppi_id, title, abstract) |> # Using only relevant variables
      mutate(
        final_human_decision = 0
      )
  }
  ) |> 
  list_rbind()

sch_rev_incl <- 
  revtools::read_bibliography("school reviews/sch_rev_incl.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 1
  )

sch_rev_ris_dat <- bind_rows(sch_rev_excl, sch_rev_incl)

ids <- sch_rev_ris_dat |> pull(eppi_id)
#sch_rev_ris_dat$eppi_id |> n_distinct() 

# Missing references in ris dat

miss_ref <- 
  screen_report_dat_raw |> 
  filter(!ItemId %in% ids)

# Seems to be non-finalized code. Most of the 9 have one screener only
screen_report_dat_filtered <- 
  screen_report_dat |> 
  filter(eppi_id %in% ids)

sch_rev_dat_wide <- 
  left_join(screen_report_dat_filtered, sch_rev_ris_dat, by = join_by(eppi_id)) |> 
  arrange(final_human_decision)

# Detecting individual screener names
screeners_var <- 
  screen_report_dat |> 
  reframe(
    name = unique(c_across(include:exclude9))
  ) |> 
  pull(name)

screeners <- 
  unlist(strsplit(screeners_var, "(?<=[a-z])(?=[A-Z])", perl = TRUE)) |> 
  gsub("\\[.*","", x = _) |> 
  unique()


filter_list <- list()

for (i in 1:length(screeners)){
  filter_list[[i]] <- 
    sch_rev_dat_wide |> 
    filter(if_any(include:exclude9, ~ str_detect(.x, screeners[i]))) |> 
    mutate(
      screener = screeners[i],
      across(include:exclude9, ~ str_extract(.x, screeners[i]))
    ) |> 
    relocate(screener)
}

single_screen_dat <- 
  filter_list |> 
  list_rbind() |> 
  mutate(
    #exclude = if_else(!is.na(exclude), screener, NA_character_),
    exclude = if_any(exclude1:exclude9, ~ !is.na(.x)),
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

sch_rev_single_perform_dat <- 
  single_screen_dat |> 
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
    review_authors = "Dietrichson et al. (2020, 2021)",
    review = "School reviews",
    role = rep(c("Author", "Assistant", "Author", "Assistant"), c(1, 3, 1, 3)),
  ) |> 
  relocate(review_authors:role) 

saveRDS(sch_rev_single_perform_dat, "single screener data/sch_rev_single_perform_dat.rds")

# Extracting all individual screener scores in wide format

single_screen_dat_wide <- 
  single_screen_dat |> 
  pivot_wider(
    id_cols = eppi_id,
    id_expand = TRUE,
    values_from = screener_decision,
    names_from = screener,
  )

sch_rev_dat <- 
  left_join(sch_rev_dat_wide, single_screen_dat_wide, by = join_by(eppi_id)) |> 
  relocate(final_human_decision, .after = last_col()) |> 
  rowwise() |> 
  mutate(
    n_screeners = sum(!is.na(c_across(`Rasmus  Klokker`:`Erika Lundqvist`)))
  ) |> 
  ungroup()

n_refs <- sch_rev_dat |> filter(n_screeners == 2 & uncertain == "") |> nrow()
saveRDS(n_refs, "single screener data/Number of References/sch_rev_n_refs.rds")

n_in <- sch_rev_dat |> filter(n_screeners == 2 & uncertain == "" & final_human_decision == 1) |> nrow()
saveRDS(n_in, "single screener data/Number of References/sch_rev_n_in.rds")

sch_rev_dat_2screen <- 
  sch_rev_dat |> 
  # Removing train data plus uncertainty decisions
  filter(n_screeners == 2 & uncertain == "") |> 
  select(-n_screeners) |> 
  pivot_longer(
    cols = `Rasmus  Klokker`:`Erika Lundqvist`,
    names_to = "screener",
    values_to = "screener_decision"
  ) |> 
  filter(!is.na(screener_decision)) |> 
  arrange(screener, final_human_decision) |>
  relocate(screener:screener_decision, .before = final_human_decision)
  

sch_rev_single_perform_dat_2screen <- 
  sch_rev_dat_2screen |> 
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
    review_authors = "Dietrichson et al. (2020, 2021)",
    review = "School reviews",
    role = rep(c("Assistant", "Author", "Assistant"), c(4, 1, 3)),
  ) |> 
  relocate(review_authors:role)

# Removing reviewer since due to few screened references that was not a part of the reviewer training (i.e., 12)
sch_rev_single_perform_dat_2screen <- 
  sch_rev_single_perform_dat_2screen |> 
  filter(screener != "Erika Lundqvist")

saveRDS(sch_rev_single_perform_dat_2screen, "single screener data/sch_rev_single_perform_dat_2screen.rds")

#------------------------------------------------------------------------------
# Old
#------------------------------------------------------------------------------
sch_rev_dat <- 
  left_join(screen_report_dat_filtered, sch_rev_ris_dat, by = join_by(eppi_id)) |> 
  select(-c(author_short, title_report)) |> 
  relocate(exclude:include, .before = final_human_decision) |> 
  arrange(final_human_decision) |> 
  mutate(
    review_authors = "Dietrichson et al. (2020, 2021)",
    review = "School reviews",
    studyid = 1:n(),
    abstract = str_remove_all(abstract, "\\<bold\\>"),
    conflict = if_else(!is.na(exclude) & !is.na(include), 1, 0)
  ) |> 
  relocate(studyid, .after = eppi_id) |> 
  relocate(author) |> 
  relocate(review_authors:review) |> 
  filter(is.na(uncertain)) |> # Removing uncertain decision
  select(-uncertain)

# Level of conflicts
sch_rev_dat |> 
  summarise(
    number_of_references = n(),
    number_of_conflicts = sum(conflict == 1), 
    percent_conflict = mean(conflict == 1),
    .by = final_human_decision
  )

saveRDS(sch_rev_dat, "all data sets/sch_rev_dat.rds")

