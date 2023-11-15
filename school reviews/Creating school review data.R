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
  html_table() 

# Removing Duplicate flaged studies
screen_report_dat_raw_filtered <- 
  screen_report_dat_raw |> 
  filter(!`I/E/D/S flag` %in% c("(S) Duplicate", "FullPath:"))

screen_report_dat_raw_filtered |> glimpse()

screen_report_dat <- 
  screen_report_dat_raw_filtered |> 
  filter(str_detect(`I/E/D/S flag`, "(I)|(E)")) |> 
  select(-`I/E/D/S flag`) |> 
  rename(
    eppi_id= ItemId,
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
  mutate(
    across(include:exclude9, ~ na_if(.x, "")),
    include = if_else(!is.na(include), "Included", NA_character_),
    exclude = if_any(exclude1:exclude9, ~ !is.na(.x)),
    exclude = if_else(exclude == TRUE, "Excluded", NA_character_)
  ) |> 
  select(-c(exclude1:exclude9, `K-6`:`No info yet`)) |> 
  relocate(exclude, .before = include) |> 
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
  screen_report_dat_raw_filtered |> 
  filter(!ItemId %in% ids)

# Seems to be non-finalized code. Most of the 9 have one screener only
screen_report_dat_filtered <- 
  screen_report_dat |> 
  filter(eppi_id %in% ids)

sch_rev_dat <- 
  left_join(screen_report_dat_filtered, sch_rev_ris_dat, by = join_by(eppi_id)) |> 
  select(-c(author_short, title_report)) |> 
  relocate(exclude:include, .before = final_human_decision) |> 
  arrange(final_human_decision) |> 
  mutate(
    review_authors = "Dietrichson et al. (2018, 2020, 2021)",
    review = "School reviews",
    studyid = 1:n(),
    abstract = str_remove_all(abstract, "\\<bold\\>"),
    conflict = if_else(!is.na(exclude) & !is.na(include), 1, 0)
  ) |> 
  relocate(studyid, .after = eppi_id) |> 
  relocate(author) |> 
  relocate(review_authors:review) |> 
  filter(is.na(uncertain)) |> # Removeing uncertain decision
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

