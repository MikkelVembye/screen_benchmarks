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
  filter(str_detect(`I/E/D/S flag`, "(I)|(E)")) |> 
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
  mutate(
    across(exclude1:include, ~ na_if(.x, "")),
    include = if_else(!is.na(include), "Included", NA_character_),
    exclude = if_any(exclude1:exclude3, ~ !is.na(.x)),
    exclude = if_else(exclude == TRUE, "Excluded", NA_character_)
  ) |> 
  select(-c(exclude1:exclude3)) |> 
  relocate(exclude, .before = include) |> 
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

child_ratio_dat <- 
  left_join(screen_report_dat, child_ratio_ris_dat, by = join_by(eppi_id)) |> 
  select(-c(author_short, title_report)) |> 
  relocate(exclude:include, .before = final_human_decision) |> 
  arrange(final_human_decision) |> 
  mutate(
    review_authors = "Dalgaard et al. (2022)",
    review = "Adult child ratio",
    studyid = 1:n(),
    abstract = str_remove_all(abstract, "\\<bold\\>"),
    conflict = if_else(!is.na(exclude) & !is.na(include), 1, 0)
  ) |> 
  relocate(studyid, .after = eppi_id) |> 
  relocate(author) |> 
  relocate(review_authors:review)

# Level of conflicts
child_ratio_dat |> 
  summarise(
    number_of_references = n(),
    number_of_conflicts = sum(conflict == 1), 
    percent_conflict = mean(conflict == 1),
    .by = final_human_decision
  )

saveRDS(child_ratio_dat, file = "all data sets/child_ratio_dat.rds")



