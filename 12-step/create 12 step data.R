# Creating data for 12-step review
library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)

html <- read_html("12-step/12step screen on title & abstract full coding report.html")

screen_report_dat_raw <- 
  html |> 
  html_element("table") |> 
  html_table()

# Til Anja: Du kan bruge glimpse() til at få overblik over variable navnene
screen_report_dat_raw |> glimpse()


# Read duplicate and delete refs (Tal med Anja)
miss_refs_ids <- readRDS("12-step/duplicate_delete_ids.rds")

miss_refs_raw <- 
  screen_report_dat_raw |> 
  filter(ItemId %in% miss_refs_ids)

boeg_refs <- 
  screen_report_dat_raw |> 
  select(-`I/E/D/S flag`) |> 
  rename(
    eppi_id= ItemId,
    author_short = ShortTitle, 
    title_report = Title, 
    exclude1 = `Exclude on intervention`, 
    exclude2 = `Exclude on participants`,
    exclude3 = `Exclude on study type`,
    exclude4 = `Exclude on review`,
    include = `Include on basis of title and abstract`
  ) |> 
  mutate(
    across(exclude1:include, ~ na_if(.x, "")),
    across(exclude1:include, ~ str_replace_all(.x, " BÃ¸g", "Boeg"))
  ) |> 
  arrange(eppi_id) |> 
  filter(eppi_id != "FullPath:") |>  # removing first row
  filter(if_any(exclude1:include, ~ str_detect(., "Boeg"))) |> 
  pull(eppi_id)


screen_report_dat <- 
  screen_report_dat_raw |> 
  select(-`I/E/D/S flag`) |> 
  rename(
    eppi_id= ItemId,
    author_short = ShortTitle, 
    title_report = Title, 
    exclude1 = `Exclude on intervention`, 
    exclude2 = `Exclude on participants`,
    exclude3 = `Exclude on study type`,
    exclude4 = `Exclude on review`,
    include = `Include on basis of title and abstract`
  ) |> 
  mutate(
    across(exclude1:include, ~ na_if(.x, "")),
    across(exclude1:include, ~ str_replace_all(.x, " BÃ¸g", "Boeg"))
  ) |> 
  arrange(eppi_id) |> 
  filter(eppi_id != "FullPath:") 


# Loading included and excluded studies
ex_paths <- list.files("12-step/", pattern = "excl")

friends_excl <- 
  map(ex_paths, ~ {
    revtools::read_bibliography(paste0("12-step/", .x)) |> 
      suppressWarnings() |> 
      as_tibble() |>
      select(author, eppi_id, title, abstract) |> # Using only relevant variables
      mutate(
        human_code = 0
      )
  }
  ) |> 
  list_rbind()

friends_incl <- 
  revtools::read_bibliography("12-step/12step_incl.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    human_code = 1
  )

step12_ris_dat <- bind_rows(friends_excl, friends_incl) 

# ids til filter screen report data
ids <- step12_ris_dat |> pull(eppi_id)

# FIND DUPLICATE AND DELETE FLAGED STUDIES
#miss_refs <- 
#  screen_report_dat |> 
#  filter(!eppi_id %in% ids) 
#
#miss_refs_ids <- 
#  screen_report_dat |> 
#  filter(!eppi_id %in% ids) |> 
#  pull(eppi_id)
#
#saveRDS(miss_refs_ids, file = "12-step/duplicate_delete_ids.rds")

# filter screen report data
screen_report_dat_filtered <- 
  screen_report_dat |> 
  # removing duplicates and delete flaged studies + single-screened studies
  filter(eppi_id %in% ids & if_any(exclude1:include, ~ str_detect(., "Boeg", negate = TRUE))) |> 
  mutate(
    include = if_else(!is.na(include), "Included", NA_character_),
    exclude = if_any(exclude1:exclude4, ~ !is.na(.x)),
    exclude = if_else(exclude == TRUE, "Excluded", NA_character_)
  ) |> 
  select(-c(exclude1:exclude4)) |> 
  relocate(exclude, .before = include)

step12_dat <- 
  left_join(screen_report_dat_filtered, step12_ris_dat, by = join_by(eppi_id)) |> 
  select(-c(author_short, title_report)) |> 
  relocate(exclude:include, .before = human_code) |> 
  arrange(human_code) |> 
  mutate(
    studyid = 1:n(),
    abstract = str_remove_all(abstract, "\\<bold\\>"),
    conflict = if_else(!is.na(exclude) & !is.na(include), 1, 0)
  ) |> 
  relocate(studyid, .after = eppi_id)

  
# Level of conflicts
step12_dat |> 
  summarise(
    number_of_references = n(),
    number_of_conflicts = sum(conflict == 1), 
    percent_conflict = mean(conflict == 1),
    .by = human_code
  )





