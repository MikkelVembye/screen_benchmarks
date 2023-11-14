library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)

html <- read_html("friends/FRIENDS Title & Abstract full coding report.html")

screen_report_dat_raw <- 
  html |> 
  html_element("table") |> 
  html_table()


screen_report_dat <- 
  screen_report_dat_raw |> 
  select(-`I/E/D/S flag`) |> 
  rename(
    eppi_id= ItemId,
    author_short = ShortTitle, 
    title_report = Title, 
    exclude = EXCLUDE, 
    include = `INCLUDE on T/A`
  ) |> 
  mutate(
    across(exclude:include, ~ na_if(.x, ""))
  ) |> 
  arrange(eppi_id) |> 
  filter(eppi_id != "FullPath:") # removing first row


# Loading included and excluded studies
friends_excl <- revtools::read_bibliography("friends/friends_excl.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 0
  )

friends_incl <- revtools::read_bibliography("friends/friends_incl.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |>
  mutate(
    final_human_decision = 1
  )

friends_raw_dat <- 
  bind_rows(filter(friends_excl, eppi_id != "91822596"), friends_incl)  # Removing one reference that both appears among in and excluded references
  

# Remove gray lit refs which has not been assessed at the title and abstract level
html2 <- read_html("friends/gray search.html")

gray_ids <- 
  html2 |> 
  html_element("table") |> 
  html_table() |> 
  select(ID) |> 
  pull(ID) |> 
  as.character()

friends_dat <- 
  left_join(screen_report_dat, friends_raw_dat, by = join_by(eppi_id)) |> 
  filter(!eppi_id %in% gray_ids) |> 
  #filter(!is.na(abstract)) |> 
  arrange(final_human_decision) |> 
  mutate(
    exclude = if_else(!is.na(exclude), "Excluded", NA_character_),
    include = if_else(!is.na(include), "Included", NA_character_),
    review_authors = "Filges et al. (In progress)",
    review = "FRIENDS",
    studyid = 1:n(),
    abstract = str_remove_all(abstract, "\\<bold\\>"),
    conflict = if_else(!is.na(exclude) & !is.na(include), 1, 0)
  ) |> 
  relocate(studyid, .after = eppi_id) |> 
  select(-c(author_short:title_report)) |> 
  relocate(author, .before = eppi_id) |> 
  relocate(exclude:include, .before = final_human_decision) |> 
  relocate(review_authors:review)

saveRDS(friends_dat, "all data sets/friends_dat.rds")

# Level of conflicts
friends_dat |> 
  summarise(
    number_of_references = n(),
    number_of_conflicts = sum(conflict == 1), 
    percent_conflict = mean(conflict == 1),
    .by = final_human_decision
  )



