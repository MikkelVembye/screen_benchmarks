# Creating data for adult child ratio review
library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)
library(readr)

html <- read_html("Adult child ratio/Child ratio screen on Title & Abstract full coding report.html")

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
    exclude1 = EXCLUDE, 
    exclude2 = `EXCLUDE Review`,
    exclude3 = `EXCLUDE Qualitative Research`,
    include = `INCLUDE on title & abstract`
  ) |> 
  arrange(eppi_id) 


# Loading included and excluded studies
ex_paths <- list.files("Adult child ratio/", pattern = "excluded_on")

child_ratio_excl <- 
  map(ex_paths, ~ {
    revtools::read_bibliography(paste0("Adult child ratio/", .x)) |> 
      suppressWarnings() |> 
      as_tibble() |>
      select(author, eppi_id, title, abstract) |> # Using only relevant variables
      mutate(
        final_human_decision = 0
      )
  }
  ) |> 
  list_rbind()

child_ratio_incl <- 
  revtools::read_bibliography("Adult child ratio/included_on_title_abstract.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 1
  )

child_ratio_ris_dat <- bind_rows(child_ratio_excl, child_ratio_incl) 

# Find studies appearing both as in- and excluded
ids <- child_ratio_ris_dat |>  summarise(n = n(), .by = eppi_id) |> filter(n > 1) |> pull(eppi_id)

# Remove double studies from excluded data
child_ratio_ris_dat <- 
  bind_rows(
    filter(child_ratio_excl, !eppi_id %in% ids),
    child_ratio_incl
  ) 


child_ratio_dat_wide <- 
  left_join(screen_report_dat, child_ratio_ris_dat, by = join_by(eppi_id)) |> 
  #filter(!is.na(exclude) & !is.na(include)) |> 
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


filter_list <- list()

for (i in 1:length(screeners)){
  filter_list[[i]] <- 
    child_ratio_dat_wide |> 
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

child_ratio_single_perform_dat <- 
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
    review_authors = "Dalgaard, Bondebjerg et al. (2022)",
    review = "Adult/child ratio",
    role = rep(c("Assistant", "Assistant", "Author"), 2),
  ) |> 
  relocate(review_authors:role)

saveRDS(child_ratio_single_perform_dat, "single screener data/child_ratio_single_perform_dat.rds")

# Extracting all individual screener scores in wide format to remove training data

single_screen_dat_wide <- 
  single_screen_dat |> 
  pivot_wider(
    id_cols = eppi_id,
    id_expand = TRUE,
    values_from = screener_decision,
    names_from = screener,
  )

child_ratio_dat <- 
  left_join(child_ratio_dat_wide, single_screen_dat_wide, by = join_by(eppi_id)) |> 
  relocate(final_human_decision, .after = last_col()) |> 
  rowwise() |> 
  mutate(
    n_screeners = sum(!is.na(c_across(`Victor Nissen`:`Anja Bondebjerg`)))
  ) |> 
  ungroup()

n_refs <- child_ratio_dat |> filter(n_screeners == 2) |> nrow()
saveRDS(n_refs, "single screener data/Number of References/child_ratio_refs.rds")

n_in <- child_ratio_dat |> filter(n_screeners == 2 & final_human_decision == 1) |> nrow()
saveRDS(n_in, "single screener data/Number of References/child_ratio_n_in.rds")

child_ratio_dat_2screen <- 
  child_ratio_dat |> 
  filter(n_screeners == 2) |> 
  select(-n_screeners) |> 
  pivot_longer(
    cols = `Victor Nissen`:`Anja Bondebjerg`,
    names_to = "screener",
    values_to = "screener_decision"
  ) |> 
  filter(!is.na(screener_decision)) |> 
  arrange(screener, final_human_decision) |>
  relocate(screener:screener_decision, .before = final_human_decision)

child_ratio_single_perform_dat_2screen <- 
  child_ratio_dat_2screen |> 
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
    review_authors = "Dalgaard, Bondebjerg et al. (2022)",
    review = "Adult/child ratio",
    role = rep(c("Author", "Assistant", "Author", "Assistant"), c(1, 3, 1, 1)),
  ) |> 
  relocate(review_authors:role)

saveRDS(child_ratio_single_perform_dat_2screen, "single screener data/child_ratio_single_perform_dat_2screen.rds")

#------------------------------------------------------------------------------
# Old
#------------------------------------------------------------------------------
#child_ratio_dat <- 
#  left_join(screen_report_dat, child_ratio_ris_dat, by = join_by(eppi_id)) |> 
#  #select(-c(author_short, title_report)) |> 
#  #relocate(exclude:include, .before = final_human_decision) |> 
#  arrange(final_human_decision) |> 
#  mutate(
#    review_authors = "Dalgaard et al. (2022)",
#    review = "Adult child ratio",
#    studyid = 1:n(),
#    abstract = str_remove_all(abstract, "\\<bold\\>"),
#    conflict = if_else(!is.na(exclude) & !is.na(include), 1, 0)
#  ) |> 
#  relocate(studyid, .after = eppi_id) |> 
#  relocate(author) |> 
#  relocate(review_authors:review)
#
## Level of conflicts
#child_ratio_dat |> 
#  summarise(
#    number_of_references = n(),
#    number_of_conflicts = sum(conflict == 1), 
#    percent_conflict = mean(conflict == 1),
#    .by = final_human_decision
#  )
#
#saveRDS(child_ratio_dat, file = "all data sets/child_ratio_dat.rds")



