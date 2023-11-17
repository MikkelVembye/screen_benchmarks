# Creating data for Outreach review
library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)

library(readr)
Screen_on_title_abstract <- read_csv("Screen on Title & Abstract full coding report (1).html")
View(Screen_on_title_abstract)


html <- read_html("Screen on Title & Abstract full coding report (1).html")

screen_report_dat_raw <- 
  html |> 
  html_element("table") |> 
  html_table()

# Til Anja: Du kan bruge glimpse() til at få overblik over variable navnene
screen_report_dat_raw |> glimpse()

# ANBO: Hertil har det fungeret fint og er kørt som det skal. Jeg har uploadet RIS og HTML-filer samt coding report. Tror ikke det er nødvendigt at fjerne deleted/duplicates her, da screeningsrapporten umiddelbart kun indeholder inkluderede studier.   

# Mikkel: Jeg kan simpelthen ikke få den næste del af scriptet til at fungere - jeg kan se RIS-filerne i mit environment, men når jeg bruger kommandoerne, giver det bare 0 observationer, som om den ikke læser filerne?

# Loading included and excluded studies

ex_paths <- list.files("Outreach/", pattern = "excl")

Excluded_on_TA_1 <- 
  map(ex_paths, ~ {
    revtools::read_bibliography(paste0("ExportedRIS.excludetitleabstract1.txt")) |> 
      suppressWarnings() |> 
      as_tibble() |>
      select(author, eppi_id, title, abstract) |> # Using only relevant variables
      mutate(
        final_human_decision = 0
      )
  }
  ) |> 
  list_rbind()

outreach_excl <- 
  map(ex_paths, ~ {
    revtools::read_bibliography(paste0("Outreach/ExportedRis.excludetitleabstract2txt")) |> 
      suppressWarnings() |> 
      as_tibble() |>
      select(author, eppi_id, title, abstract) |> # Using only relevant variables
      mutate(
        final_human_decision = 0
      )
  }
  ) |> 
  list_rbind()

outreach_excl <- 
  map(ex_paths, ~ {
    revtools::read_bibliography(paste0("Outreach/ExportedRis.excludetitleabstract3txt")) |> 
      suppressWarnings() |> 
      as_tibble() |>
      select(author, eppi_id, title, abstract) |> # Using only relevant variables
      mutate(
        final_human_decision = 0
      )
  }
  ) |> 
  list_rbind()

ex_paths <- list.files("Outreach/", pattern = "incl")


outreach_incl <- 
  map(ex_paths, ~ {
    revtools::read_bibliography("Outreach/ExportedRis_includedtitleabstract") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 1
  )

    
  }
  ) |> 
  list_rbind()

outreach_ris_dat <- bind_rows(outreach_excl, outreach_incl) 

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
  relocate(exclude:include, .before = final_human_decision) |> 
  arrange(final_human_decision) |> 
  mutate(
    review_authors = "Bøg et al. (2017)",
    review = "12-step",
    studyid = 1:n(),
    abstract = str_remove_all(abstract, "\\<bold\\>"),
    conflict = if_else(!is.na(exclude) & !is.na(include), 1, 0)
  ) |> 
  relocate(studyid, .after = eppi_id) |> 
  relocate(author) |> 
  relocate(review_authors:review)


# Level of conflicts
step12_dat |> 
  summarise(
    number_of_references = n(),
    number_of_conflicts = sum(conflict == 1), 
    percent_conflict = mean(conflict == 1),
    .by = final_human_decision
  )

saveRDS(step12_dat, file = "all data sets/step12_dat.rds")




