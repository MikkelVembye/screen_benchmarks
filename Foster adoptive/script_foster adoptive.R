# Creating data for foster adoptive review
library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)
library(readr)


html <- read_html("Foster adoptive/Foster screen on Title & Abstract full coding report.html")

screen_report_dat_raw <- 
  html |> 
  html_element("table") |> 
  html_table() |> 
  filter(str_detect(`I/E/D/S flag`, "(I)|(E)"))

# Til Anja: Du kan bruge glimpse() til at få overblik over variable navnene
screen_report_dat_raw |> glimpse()

screen_report_dat <- 
  screen_report_dat_raw |> 
  select(-`I/E/D/S flag`) |> 
  rename(
    eppi_id= ItemId,
    author_short = ShortTitle, 
    title_report = Title, 
    include = INCLUDE,
    exclude1 = EXCLUDE, 
    exclude2 = `EXCLUDE: Review`
  ) |> 
  mutate(
    across(include:exclude2, ~ na_if(.x, ""))
    #include = if_else(!is.na(include), "Included", NA_character_),
  ) |> 
  #select(-c(exclude1:exclude2)) |> 
  #relocate(exclude, .before = include) |> 
  arrange(eppi_id) 

# Loading included and excluded studies
ex_paths <- list.files("Foster adoptive/", pattern = "excluded_")

foster_excl <- 
  map(ex_paths, ~ {
    revtools::read_bibliography(paste0("Foster adoptive/", .x)) |> 
      suppressWarnings() |> 
      as_tibble() |>
      select(author, eppi_id, title, abstract) |> # Using only relevant variables
      mutate(
        final_human_decision = 0
      )
  }
  ) |> 
  list_rbind()

foster_incl <- 
  revtools::read_bibliography("Foster adoptive/included_on_title_abstract.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 1
  )

foster_ris_dat <- bind_rows(foster_excl, foster_incl) 

foster_dat_wide <- 
  left_join(screen_report_dat, foster_ris_dat, by = join_by(eppi_id)) |> 
  #filter(!is.na(exclude) & !is.na(include)) |> 
  mutate(
    across(include:exclude2, ~ str_remove_all(.x, "JÃ¸rgensen"))
  ) |> 
  arrange(final_human_decision)

# Continue here:
screeners <- c("Anton Dam", "Tanne Ebert", "Nina Thorup Dalgaard", "Julie Haatuft", "Trine Filges")

filter_list <- list()

for (i in 1:5){
  filter_list[[i]] <- 
    foster_dat_wide |> 
    filter(if_any(include:exclude2, ~ str_detect(.x, screeners[i]))) |> 
    mutate(
      screener = screeners[i],
      across(include:exclude2, ~ str_extract(.x, screeners[i]))
    ) |> 
    relocate(screener)
}

single_screen_dat <- 
  filter_list |> 
  list_rbind() |> 
  mutate(
    exclude = if_any(exclude1:exclude2, ~ !is.na(.x)),
    exclude = if_else(exclude == TRUE, screener, NA_character_),
    screener_decision = case_when(
      !is.na(include) ~ 1,
      !is.na(exclude) ~ 0,
      TRUE ~ NA_real_
    ),
    conflict = if_else(screener_decision != final_human_decision, 1, 0)
  ) |> 
  select(-c(exclude1:exclude2)) |> 
  relocate(exclude, .before = include) |> 
  relocate(screener_decision, .before = final_human_decision) |> 
  filter(!if_all(exclude:include, ~ !is.na(.x))) # Removing all reference where the same screener has voted both in and out 


foster_single_perform_dat <- 
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
    review_authors = "Dalgaard, Filges et al. (2022)",
    review = "Parenting interventions",
    role = c("Assistant", "Assistant", "Author", "Assistant", "Author"),
  ) |> 
  relocate(review_authors:role)

saveRDS(foster_single_perform_dat, "single screener data/foster_single_perform_dat.rds")

# Extracting all individual screener scores in wide format

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

foster_dat <- 
  left_join(foster_dat_wide, single_screen_dat_wide, by = join_by(eppi_id)) |> 
  relocate(final_human_decision, .after = last_col()) |> 
  rowwise() |> 
  mutate(
    n_screeners = sum(!is.na(c_across(`Anton Dam`:`Trine Filges`)))
  ) |> 
  ungroup()

# References screened
n_refs <- foster_dat |> filter(n_screeners == 2) |> nrow()
saveRDS(n_refs, "single screener data/Number of References/foster_n_refs.rds")


foster_dat_2screen <- 
  foster_dat |> 
  filter(n_screeners == 2) |>
  select(-n_screeners) |> 
  pivot_longer(
    cols = `Anton Dam`:`Trine Filges`,
    names_to = "screener",
    values_to = "screener_decision"
  ) |> 
  filter(!is.na(screener_decision)) |> 
  arrange(screener, final_human_decision) |>
  relocate(screener:screener_decision, .before = final_human_decision)

foster_single_perform_dat_2screen <- 
  foster_dat_2screen |> 
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
    review_authors = "Dalgaard, Filges et al. (2022)",
    review = "Parenting interventions",
    role = c("Assistant", "Assistant", "Author", "Assistant", "Author")
  ) |> 
  relocate(review_authors:role)

saveRDS(foster_single_perform_dat_2screen, "single screener data/foster_single_perform_dat_2screen.rds")




