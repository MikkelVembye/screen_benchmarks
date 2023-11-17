# Creating data for adult child ratio qualitative
library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)
library(readr)


html <- read_html("Adult child ratio qualitative/Caregiver screen on Title & Abstract full coding report.html")

screen_report_dat_raw <- 
  html |> 
  html_element("table") |> 
  html_table() |> 
  filter(str_detect(`I/E/D/S flag`, "(I)|(E)"))

# Til Anja: Du kan bruge glimpse() til at få overblik over variable navnene
screen_report_dat_raw |> glimpse()


# Loading included and excluded studies

screen_report_dat <- 
  screen_report_dat_raw |> 
  select(-`I/E/D/S flag`) |> 
  rename(
    eppi_id= ItemId,
    author_short = ShortTitle, 
    title_report = Title, 
    include = `INCLUDE on title & abstract`,
    exclude = EXCLUDE
  ) |> 
  mutate(
    across(include:exclude, ~ na_if(.x, "")),
    include = if_else(!is.na(include), "Included", NA_character_),
    exclude = if_any(exclude, ~ !is.na(.x)),
    exclude = if_else(exclude == TRUE, "Excluded", NA_character_)
  ) |> 
  relocate(exclude, .before = include) |> 
  arrange(eppi_id) 

# Loading included and excluded studies

caregiver_ratio_excl <- 
  revtools::read_bibliography("Adult child ratio qualitative/excluded_on_title_abstract.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 0
  )

caregiver_ratio_incl <- 
  revtools::read_bibliography("Adult child ratio qualitative/included_on_title_abstract.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 1
  )

caregiver_ratio_ris_dat <- bind_rows(caregiver_ratio_excl, caregiver_ratio_incl) 

caregiver_ratio_dat <- 
  left_join(screen_report_dat, caregiver_ratio_ris_dat, by = join_by(eppi_id)) |> 
  select(-c(author_short, title_report)) |> 
  relocate(exclude:include, .before = final_human_decision) |> 
  arrange(final_human_decision) |> 
  mutate(
    review_authors = "Dalgaard, Bondebjerg, & Svinth (2022)",
    review = "Caregiver/child ratio qualitative",
    studyid = 1:n(),
    abstract = str_remove_all(abstract, "\\<bold\\>"),
    conflict = if_else(!is.na(exclude) & !is.na(include), 1, 0)
  ) |> 
  relocate(studyid, .after = eppi_id) |> 
  relocate(author) |> 
  relocate(review_authors:review)

# Level of conflicts
caregiver_ratio_dat |> 
  summarise(
    number_of_references = n(),
    number_of_conflicts = sum(conflict == 1), 
    percent_conflict = mean(conflict == 1),
    .by = final_human_decision
  )

saveRDS(caregiver_ratio_dat, file = "all data sets/caregiver_ratio_dat.rds")
