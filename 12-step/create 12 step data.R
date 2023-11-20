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
  html_table() |> 
  filter(str_detect(`I/E/D/S flag`, "(I)|(E)"))

# Til Anja: Du kan bruge glimpse() til at få overblik over variable navnene
screen_report_dat_raw |> glimpse()


# Read duplicate and delete refs (Tal med Anja)
#miss_refs_ids <- readRDS("12-step/duplicate_delete_ids.rds")
#
#miss_refs_raw <- 
#  screen_report_dat_raw |> 
#  filter(ItemId %in% miss_refs_ids)

# All studies coded by Martin Boeg was single screened. 
# Therefore, we exclude these references
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
    across(exclude1:include, ~ str_replace_all(.x, "BÃ¸g", "Boeg"))
  ) |> 
  arrange(eppi_id) |> 
  #filter(eppi_id != "FullPath:") |>  # removing first row
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
    across(exclude1:include, ~ str_replace_all(.x, " BÃ¸g", "Boeg"))
  ) |> 
  arrange(eppi_id) 

# Loading included and excluded studies
ex_paths <- list.files("12-step/", pattern = "excl")

step12_excl <- 
  map(ex_paths, ~ {
    revtools::read_bibliography(paste0("12-step/", .x)) |> 
      suppressWarnings() |> 
      as_tibble() |>
      select(author, eppi_id, title, abstract) |> # Using only relevant variables
      mutate(
        final_human_decision = 0
      )
  }
  ) |> 
  list_rbind()

step12_incl <- 
  revtools::read_bibliography("12-step/12step_incl.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 1
  )

step12_ris_dat <- bind_rows(step12_excl, step12_incl) 


step12_dat_wide <- 
  left_join(screen_report_dat, step12_ris_dat, by = join_by(eppi_id)) |> 
  filter(!eppi_id %in% boeg_refs) |> 
  arrange(final_human_decision)

# Detecting individual screener names
screeners_var <- 
  step12_dat_wide |> 
  reframe(
    name = unique(c_across(exclude1:include))
  ) |> 
  pull(name)

screeners <- 
  unlist(strsplit(screeners_var, "(?<=[a-z])(?=[A-Z])", perl = TRUE)) |> 
  gsub("\\[.*","", x = _) |> 
  unique() 

filter_list <- list()

for (i in 1:length(screeners)){
  filter_list[[i]] <- 
    step12_dat_wide |> 
    filter(if_any(exclude1:include, ~ str_detect(.x, screeners[i]))) |> 
    mutate(
      screener = screeners[i],
      across(exclude1:include, ~ str_extract(.x, screeners[i]))
    ) |> 
    relocate(screener)
}

single_screen_dat <- 
  filter_list |> 
  list_rbind() |> 
  mutate(
    exclude = if_any(exclude1:exclude4, ~ !is.na(.x)),
    exclude = if_else(exclude == TRUE, screener, NA_character_),
    screener_decision = case_when(
      !is.na(include) ~ 1,
      !is.na(exclude) ~ 0,
      !is.na(include) & !is.na(exclude) ~ 2, # Indicating non decisive answer
      TRUE ~ NA_real_
    ),
    conflict = if_else(screener_decision != final_human_decision, 1, 0)
  ) |> 
  select(-c(exclude1:exclude4)) |> 
  relocate(exclude, .before = include) |> 
  relocate(screener_decision, .before = final_human_decision)

step12_single_perform_dat <- 
  single_screen_dat |> 
  filter(screener %in% screeners[1:2]) |> 
  summarise(
    TP = sum(screener_decision == 1 & final_human_decision == 1),
    TN = sum(screener_decision == 0 & final_human_decision == 0),
    FN = sum(screener_decision == 0 & final_human_decision == 1),
    FP = sum(screener_decision == 1 & final_human_decision == 0),
    recall = TP / (TP + FN),
    spec = TN / (TN + FP),
    bacc = (recall + spec) / 2,
    .by = screener
  ) |> 
  mutate(
    review_authors = "Bøg, Filges et al. (2017)",
    review = "12-step programs",
    role = c("Author", "Assistant"),
  ) |> 
  relocate(review_authors:role)


#------------------------------------------------------------------------------
# Old
#------------------------------------------------------------------------------


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




