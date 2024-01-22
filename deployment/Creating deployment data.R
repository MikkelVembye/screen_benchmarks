# Creating data for Deployment review
library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)

html <- read_html("deployment/Deployment screen on Title & Abstract full coding report(2).html")

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
    exclude = EXCLUDE, 
    include = `INCLUDE on title & abstract`
  ) |> 
  mutate(
    across(exclude:include, ~ str_replace_all(.x, "amanda weber", "Amanda Weber"))
  ) |> 
  arrange(eppi_id)

rm(screen_report_dat_raw)

# Loading included and excluded studies
deploy_excl <- revtools::read_bibliography("deployment/deploy_excl.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 0
  )

deploy_incl <- revtools::read_bibliography("deployment/deploy_incl.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |>
  mutate(
    final_human_decision = 1
  )

deploy_ris_dat <- bind_rows(deploy_excl, deploy_incl)

deploy_dat_wide <- 
  left_join(screen_report_dat, deploy_ris_dat, by = join_by(eppi_id)) |> 
  arrange(final_human_decision)

# Detecting individual screener names
screeners_var <- 
  screen_report_dat |> 
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
    deploy_dat_wide |> 
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

deploy_single_perform_dat <- 
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
    review_authors = "Bøg, Filges, et al. (2018)",
    review = "Deployment",
    role = rep(c("Author", "Assistant"), c(1,2)),
  ) |> 
  relocate(review_authors:role) 
  

saveRDS(deploy_single_perform_dat, "single screener data/All screenings/deploy_single_perform_dat.rds")

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

deploy_dat <- 
  left_join(deploy_dat_wide, single_screen_dat_wide, by = join_by(eppi_id)) |> 
  relocate(final_human_decision, .after = last_col()) |> 
  rowwise() |> 
  mutate(
    n_screeners = sum(!is.na(c_across(`Ida Rasmussen`:`Trine Filges`)))
  ) |> 
  ungroup()

n_refs <- deploy_dat |> filter(n_screeners == 2) |> nrow()
saveRDS(n_refs, "single screener data/Number of References/deploy_n_refs.rds")

deploy_dat_2screen <- 
  deploy_dat |> 
  filter(n_screeners == 2) |>
  select(-n_screeners) |> 
  pivot_longer(
    cols = `Ida Rasmussen`:`Trine Filges`,
    names_to = "screener",
    values_to = "screener_decision"
  ) |> 
  filter(!is.na(screener_decision)) |> 
  arrange(screener, final_human_decision) |>
  relocate(screener:screener_decision, .before = final_human_decision)

deploy_single_perform_dat_2screen <- 
  deploy_dat_2screen |> 
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
    review_authors = "Bøg, Filges, et al. (2018)",
    review = "Deployment",
    role = "Assistant",
  ) |> 
  relocate(review_authors:role)

saveRDS(deploy_single_perform_dat_2screen, "single screener data/deploy_single_perform_dat_2screen.rds")



#------------------------------------------------------------------------------
# Old
#------------------------------------------------------------------------------
deploy_dat <- 
  left_join(screen_report_dat, deploy_ris_dat, by = join_by(eppi_id)) |> 
  arrange(final_human_decision) |> 
  mutate(
    exclude = if_else(!is.na(exclude), "Excluded", NA_character_),
    include = if_else(!is.na(include), "Included", NA_character_),
    review_authors = "Bøg et al. (2018)",
    review = "Deployment",
    studyid = 1:n(),
    abstract = str_remove_all(abstract, "\\<bold\\>"),
    conflict = if_else(!is.na(exclude) & !is.na(include), 1, 0)
  ) |> 
  relocate(studyid, .after = eppi_id) |> 
  select(-c(author_short:title_report)) |> 
  relocate(author, .before = eppi_id) |> 
  relocate(exclude:include, .before = final_human_decision) |> 
  relocate(review_authors:review)

saveRDS(deploy_dat, "all data sets/deploy_dat.rds")

# Level of conflicts
deploy_dat |> 
  summarise(
    number_of_references = n(),
    number_of_conflicts = sum(conflict == 1), 
    percent_conflict = mean(conflict == 1),
    .by = final_human_decision
  )
