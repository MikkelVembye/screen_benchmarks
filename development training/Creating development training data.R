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


dev_train_dat_wide <- 
  left_join(screen_report_dat_filtered, dev_train_ris_dat, by = join_by(eppi_id)) |> 
  arrange(final_human_decision)

# Detecting individual screener names
screeners_var <- 
  screen_report_dat_filtered |> 
  reframe(
    name = unique(c_across(exclude:include))
  ) |> 
  pull(name)

screeners <- 
  unlist(strsplit(screeners_var, "(?<=[a-z])(?=[A-Z])", perl = TRUE)) |> 
  gsub("\\[.*","", x = _) |> 
  unique()

filter_list <- list()

for (i in 1:length(screeners)){
  filter_list[[i]] <- 
    dev_train_dat_wide |> 
    filter(if_any(exclude:include, ~ str_detect(.x, screeners[i]))) |> 
    mutate(
      screener = screeners[i],
      across(exclude:include, ~ str_extract(.x, screeners[i]))
    ) |> 
    relocate(screener)
}

single_screen_dat <- 
  filter_list |> 
  list_rbind() |> 
  mutate(
    exclude = if_else(!is.na(exclude), screener, NA_character_),
    #exclude = if_any(exclude:include, ~ !is.na(.x)),
    #exclude = if_else(exclude == TRUE, screener, NA_character_),
    screener_decision = case_when(
      !is.na(include) ~ 1,
      !is.na(exclude) ~ 0,
      !is.na(include) & !is.na(exclude) ~ 2, # Indicating non decisive answer
      TRUE ~ NA_real_
    ),
    conflict = if_else(screener_decision != final_human_decision, 1, 0)
  ) |> 
  relocate(exclude, .before = include) |> 
  relocate(screener_decision, .before = final_human_decision)

dev_train_single_perform_dat <- 
  single_screen_dat |> 
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
    review_authors = "Filges, Torgerson, et al. (2019)",
    review = "Development Training",
    role = rep(c("Author", "Assistant", "Author"), c(1,1,3))
  ) |> 
  relocate(review_authors:role) 

saveRDS(dev_train_single_perform_dat , "single screener data/dev_train_single_perform_dat.rds")

#----------------------------------------------------------------------------------------
# Extracting all individual screener scores in wide format to exclude training references
#----------------------------------------------------------------------------------------

single_screen_dat_wide <- 
  single_screen_dat |> 
  pivot_wider(
    id_cols = eppi_id,
    id_expand = TRUE,
    values_from = screener_decision,
    names_from = screener,
  )

dev_train_dat <- 
  left_join(dev_train_dat_wide, single_screen_dat_wide, by = join_by(eppi_id)) |> 
  relocate(final_human_decision, .after = last_col()) |> 
  rowwise() |> 
  mutate(
    n_screeners = sum(!is.na(c_across(`Louise Gascoine`:`Trine Filges`)))
  ) |> 
  ungroup()

dev_train_dat_2screen <- 
  dev_train_dat |> 
  filter(n_screeners == 2) |>
  select(-n_screeners) |> 
  pivot_longer(
    cols = `Louise Gascoine`:`Trine Filges`,
    names_to = "screener",
    values_to = "screener_decision"
  ) |> 
  filter(!is.na(screener_decision)) |> 
  arrange(screener, final_human_decision) |>
  relocate(screener:screener_decision, .before = final_human_decision)

dev_train_single_perform_dat_2screen <- 
  dev_train_dat_2screen |> 
  summarise(
    TP = sum(screener_decision == 1 & final_human_decision == 1, na.rm = TRUE),
    TN = sum(screener_decision == 0 & final_human_decision == 0, na.rm = TRUE),
    FN = sum(screener_decision == 0 & final_human_decision == 1, na.rm = TRUE),
    FP = sum(screener_decision == 1 & final_human_decision == 0, na.rm = TRUE),
    recall = TP / (TP + FN),
    spec = TN / (TN + FP),
    bacc = (recall + spec) / 2,
    .by = screener
  ) |> 
  ungroup() |> 
  mutate(
    review_authors = "Filges, Torgerson, et al. (2019)",
    review = "Development Training",
    role = rep(c("Author", "Assistant", "Author"), c(2,1,2))
  ) |> 
  relocate(review_authors:role)

saveRDS(dev_train_single_perform_dat_2screen, "single screener data/dev_train_single_perform_dat_2screen.rds")

#------------------------------------------------------------------------------
# Old
#------------------------------------------------------------------------------

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

