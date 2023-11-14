# Creating data class size
library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)

html <- read_html("class size/class size screening on title and abstract full coding report.html")

screen_report_dat_raw <- 
  html |> 
  html_element("table") |> 
  html_table()

# Til Anja: Du kan bruge glimpse() til at få overblik over variable navnene
screen_report_dat_raw |> glimpse()

screen_report_dat <- 
  screen_report_dat_raw |> 
  filter(str_detect(`I/E/D/S flag`, "(I)|(E)")) |> 
  select(-`I/E/D/S flag`) |> 
  rename(
    eppi_id= ItemId,
    author_short = ShortTitle, 
    title_report = Title, 
    include = Include,
    exclude1 = Uncertain, 
    exclude2 = Exclude,
    exclude3 = `Exclude on Intervention`,
    exclude4 = `Exclude on participants`,
    exclude5 = `Exclude on study type`,
    exclude6 = `Exclude on review`,
    
  ) |> 
  mutate(
    across(include:exclude6, ~ na_if(.x, "")),
    include = if_else(!is.na(include), "Included", NA_character_),
    exclude = if_any(exclude1:exclude6, ~ !is.na(.x)),
    exclude = if_else(exclude == TRUE, "Excluded", NA_character_)
  ) |> 
  select(-c(exclude1:exclude6)) |> 
  relocate(exclude, .before = include) |> 
  arrange(eppi_id) 

# Loading included and excluded studies
ex_paths <- list.files("class size/", pattern = "excl")

cl_size_excl <- 
  map(ex_paths, ~ {
    revtools::read_bibliography(paste0("class size/", .x)) |> 
      suppressWarnings() |> 
      as_tibble() |>
      select(author, eppi_id, title, abstract) |> # Using only relevant variables
      mutate(
        final_human_decision = 0
      )
  }
  ) |> 
  list_rbind()

cl_size_incl <- 
  revtools::read_bibliography("class size/class_size_incl.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 1
  )

cl_size_ris_dat <- 
  bind_rows(filter(cl_size_excl, eppi_id != 29514377), cl_size_incl) # remove reference that appear both in in and exclude

ids <- cl_size_ris_dat |> pull(eppi_id)

# Not among ris-file references
missing_stud <- 
  screen_report_dat_raw |> 
  filter(!ItemId %in% ids & !str_detect(`I/E/D/S flag`, "(S)"))

# Removing the 4 references that does not appear among ris-file references
screen_report_dat_filtered <- 
  screen_report_dat |> 
  filter(eppi_id %in% ids)

cl_size_dat <- 
  left_join(screen_report_dat_filtered, cl_size_ris_dat, by = join_by(eppi_id)) |> 
  select(-c(author_short, title_report)) |> 
  relocate(exclude:include, .before = final_human_decision) |> 
  arrange(final_human_decision) |> 
  mutate(
    review_authors = "Filges et al. (2018)",
    review = "Class size",
    studyid = 1:n(),
    abstract = str_remove_all(abstract, "\\<bold\\>"),
    conflict = if_else(!is.na(exclude) & !is.na(include), 1, 0)
  ) |> 
  relocate(studyid, .after = eppi_id) |> 
  relocate(author) |> 
  relocate(review_authors:review)

# Level of conflicts
cl_size_dat |> 
  summarise(
    number_of_references = n(),
    number_of_conflicts = sum(conflict == 1), 
    percent_conflict = mean(conflict == 1),
    .by = final_human_decision
  )

saveRDS(cl_size_dat, file = "all data sets/cl_size_dat.rds")
