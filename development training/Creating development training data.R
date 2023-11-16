# Creating data for deveoplement training review
library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)

html <- read_html("development training/Development screen on Title & Abstract full coding report.html")

screen_report_dat_raw <- 
  html |> 
  html_element("table") |> 
  html_table() |> 
  filter(str_detect(`I/E/D/S flag`, "(I)|(E)"))

screen_report_dat <- 
  screen_report_dat_raw |> 
  select(-`I/E/D/S flag`) |> 
  rename(
    eppi_id= ItemId,
    author_short = ShortTitle, 
    title_report = Title, 
    exclude = `EXCLUDE on title & abstract`, 
    include = `INCLUDE on title & abstract`
  ) |> 
  mutate(
    across(exclude:include, ~ na_if(.x, ""))
  ) |> 
  arrange(eppi_id)

# Loading included and excluded studies
ex_paths <- list.files("development training/", pattern = "excl")

dev_train_excl <- 
  map(ex_paths, ~ {
    revtools::read_bibliography(paste0("development training/", .x)) |> 
      suppressWarnings() |> 
      as_tibble() |>
      select(author, eppi_id, title, abstract) |> # Using only relevant variables
      mutate(
        final_human_decision = 0
      )
  }
  ) |> 
  list_rbind()

dev_train_incl <- revtools::read_bibliography("development training/dev_train_incl.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |>
  mutate(
    final_human_decision = 1
  )

dev_train_ris_dat <- bind_rows(dev_train_excl, dev_train_incl) 

# Find reference appearing in both in- and excluded group
dev_train_ris_dat |> 
  summarise(
    n = n(),
    .by = eppi_id
  ) |> 
  filter(n > 1)

dev_train_ris_dat <- 
  bind_rows(filter(dev_train_excl, eppi_id != "38664832"), dev_train_incl) |> 
  mutate(
    eppi_id = if_else(str_detect(eppi_id, "29435870"), "29435870", eppi_id)
  )

ids <- dev_train_ris_dat |> pull(eppi_id)

screen_report_dat_filtered <- 
  screen_report_dat |> 
  filter(eppi_id %in% ids)

#ids_screen_rep <- screen_report_dat_filtered |> pull(eppi_id)
#dev_train_ris_dat |> filter(!eppi_id %in% ids_screen_rep) |> View()

dev_train_dat <- 
  left_join(screen_report_dat_filtered, dev_train_ris_dat, by = join_by(eppi_id)) |> 
  arrange(final_human_decision) |> 
  mutate(
    exclude = if_else(!is.na(exclude), "Excluded", NA_character_),
    include = if_else(!is.na(include), "Included", NA_character_),
    review_authors = "Filges, Torgerson et al. (2019)",
    review = "Professional Development Review",
    studyid = 1:n(),
    abstract = str_remove_all(abstract, "\\<bold\\>"),
    conflict = if_else(!is.na(exclude) & !is.na(include), 1, 0)
  ) |> 
  relocate(studyid, .after = eppi_id) |> 
  select(-c(author_short:title_report)) |> 
  relocate(author, .before = eppi_id) |> 
  relocate(exclude:include, .before = final_human_decision) |> 
  relocate(review_authors:review)

saveRDS(dev_train_dat, "all data sets/dev_train_dat.rds")

# Level of conflicts
dev_train_dat |> 
  summarise(
    number_of_references = n(),
    number_of_conflicts = sum(conflict == 1), 
    percent_conflict = mean(conflict == 1),
    .by = final_human_decision
  )

