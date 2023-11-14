library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)

html <- read_html("Asylum/Asylum screening level 1 full coding report.html")

screen_report_dat_raw <- 
  html |> 
  html_element("table") |> 
  html_table()

screen_report_dat_raw |> glimpse()

screen_report_dat <- 
  screen_report_dat_raw |> 
  filter(str_detect(`I/E/D/S flag`, "(I)|(E)")) |> 
  select(-`I/E/D/S flag`) |> 
  rename(
    eppi_id= ItemId,
    author_short = ShortTitle, 
    title_report = Title, 
    exclude1 = `Exclude on topic`, 
    exclude2 = `Exclude on participants`,
    exclude3 = `Exclude on study design`,
    include = `Included on title and abstract`
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
ex_paths <- list.files("Asylum/", pattern = "excl")

asylum_excl <- 
  map(ex_paths, ~ {
    revtools::read_bibliography(paste0("Asylum/", .x)) |> 
      suppressWarnings() |> 
      as_tibble() |>
      select(author, eppi_id, title, abstract) |> # Using only relevant variables
      mutate(
        final_human_decision = 0
      )
  }
  ) |> 
  list_rbind()

asylum_incl <- 
  revtools::read_bibliography("Asylum/asylum_incl.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 1
  )

asylum_ris_dat <- bind_rows(asylum_excl, asylum_incl) 

asylum_dat <- 
  left_join(screen_report_dat, asylum_ris_dat, by = join_by(eppi_id)) |> 
  select(-c(author_short, title_report)) |> 
  relocate(exclude:include, .before = final_human_decision) |> 
  arrange(final_human_decision) |> 
  mutate(
    review_authors = "Filges, Montgomery et al. (2015)",
    review = "Asylum",
    studyid = 1:n(),
    abstract = str_remove_all(abstract, "\\<bold\\>"),
    conflict = if_else(!is.na(exclude) & !is.na(include), 1, 0)
  ) |> 
  relocate(studyid, .after = eppi_id) |> 
  relocate(author) |> 
  relocate(review_authors:review)

# Level of conflicts
asylum_dat |> 
  summarise(
    number_of_references = n(),
    number_of_conflicts = sum(conflict == 1), 
    percent_conflict = mean(conflict == 1),
    .by = final_human_decision
  )

saveRDS(asylum_dat, file = "all data sets/asylum_dat.rds")
