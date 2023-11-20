# Creating data for class size in special education
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
  html_table() |> 
  filter(str_detect(`I/E/D/S flag`, "(I)|(E)"))

# Til Anja: Du kan bruge glimpse() til at få overblik over variable navnene
screen_report_dat_raw |> glimpse()

# ANBO: Hertil har det fungeret fint og er kørt som det skal. Jeg har uploadet RIS-filer og coding report. Tror ikke det er nødvendigt at fjerne deleted/duplicates her, da screeningsrapporten umiddelbart kun indeholder inkluderede studier.   

screen_report_dat <- 
  screen_report_dat_raw |> 
  select(-`I/E/D/S flag`) |> 
  rename(
    eppi_id= ItemId,
    author_short = ShortTitle, 
    title_report = Title, 
    exclude = EXCLUDE, 
    include = `INCLUDE on title & abstract`
  ) |> 
  mutate(
    across(exclude:include, ~ na_if(.x, ""))
  ) |> 
  arrange(eppi_id) |> 
  filter(eppi_id != "FullPath:") # removing first row


# Loading included and excluded studies
classsizespecialeducation_excl1 <- revtools::read_bibliography("Excludetitleabstract1.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 0
  )

classsizespecialeducation_excl2 <- revtools::read_bibliography("Excludetitleabstract2.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 0
  )

classsizespecialeducation_excl3 <- revtools::read_bibliography("Excludetitleabstract3.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 0
  )

classsizespecialeducation_excl4 <- revtools::read_bibliography("Excludetitleabstract4.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 0
  )

classsizespecialeducation_incl <- revtools::read_bibliography("Includetitleabstract.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |>
  mutate(
    final_human_decision = 1
  )

classsizeinspecialeducation_raw_dat_excl <- 
  bind_rows(classsizespecialeducation_excl1, classsizespecialeducation_excl2, classsizespecialeducation_excl3, classsizespecialeducation_excl4)  


classsizeinspecialeducation_raw_dat_incl <- 
  bind_rows(classsizespecialeducation_incl)
            
classsizeinspecialeducation_raw_dat_full <- 
  bind_rows(classsizeinspecialeducation_raw_dat_excl, classsizeinspecialeducation_raw_dat_incl)  


classsizeinspecialeducation_dat <- 
  left_join(screen_report_dat, classsizeinspecialeducation_raw_dat_full, by = join_by(eppi_id)) |> 
    arrange(final_human_decision) |> 
  mutate(
    exclude = if_else(!is.na(exclude), "Excluded", NA_character_),
    include = if_else(!is.na(include), "Included", NA_character_),
    conflict = if_else(!is.na(exclude) & !is.na(include), 1, 0))
   
  

saveRDS(classsizeinspecialeducation_dat, "classsizeinspecialeducation_dat_rds")

# Level of conflicts
classsizeinspecialeducation_dat |> 
  summarise(
    number_of_references = n(),
    number_of_conflicts = sum(conflict == 1), 
    percent_conflict = mean(conflict == 1),
    .by = final_human_decision
  )



