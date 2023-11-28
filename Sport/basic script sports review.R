# Creating data for sports review
library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)
library(readr)

html <- read_html("Sport/Sport screen on title & abstract full coding report.html")

screen_report_dat_raw <- 
  html |> 
  html_element("table") |> 
  html_table() |> 
  filter(str_detect(`I/E/D/S flag`, "(I)|(E)"))

screen_report_dat_raw |> glimpse()

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
  arrange(eppi_id) 

# Loading included and excluded studies
ex_paths <- list.files("Sport/", pattern = "excl")

sport_excl <- 
  map(ex_paths, ~ {
    revtools::read_bibliography(paste0("Sport/", .x)) |> 
      suppressWarnings() |> 
      as_tibble() |>
      select(author, eppi_id, title, abstract) |> # Using only relevant variables
      mutate(
        final_human_decision = 0
      )
  }
  ) |> 
  list_rbind()


sport_incl <- 
  revtools::read_bibliography("Sport/includeTA.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 1
  )

sport_ris_dat <- bind_rows(sport_excl, sport_incl) 
rm(sport_excl, sport_incl)

sport_dat_wide <- 
  left_join(screen_report_dat, sport_ris_dat, by = join_by(eppi_id)) |> 
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
    sport_dat_wide |> 
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
    #exclude = if_any(exclude1:exclude3, ~ !is.na(.x)),
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

# Check
sum(single_screen_dat$screener_decision == 2)

# All screenings including training sessions
sport_single_perform_dat <- 
  single_screen_dat |> 
  filter(screener != "VIVE Campbell") |> # Removing single double-screening
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
    review_authors = "Filges, Verner, et al. (2023)",
    review = "Organised Sport",
    role = c("Author", "Assistant", "Assistant")
  ) |> 
  relocate(review_authors:role) 

saveRDS(sport_single_perform_dat, "single screener data/All screenings/sport_single_perform_dat.rds")

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

sport_dat <- 
  left_join(sport_dat_wide, single_screen_dat_wide, by = join_by(eppi_id)) |> 
  relocate(final_human_decision, .after = last_col()) |> 
  filter(is.na(`VIVE Campbell`)) |> 
  select(-`VIVE Campbell`) |> 
  rowwise() |> 
  mutate(
    n_screeners = sum(!is.na(c_across(`Trine Filges`:`Anja Bondebjerg`)))
  ) |> 
  ungroup()

n_refs <- sport_dat |> filter(n_screeners == 2) |> nrow()
saveRDS(n_refs, "single screener data/Number of References/sport_n_refs.rds")

sport_dat_2screen <- 
  sport_dat |> 
  filter(n_screeners == 2) |>
  select(-n_screeners) |> 
  pivot_longer(
    cols = `Trine Filges`:`Anja Bondebjerg`,
    names_to = "screener",
    values_to = "screener_decision"
  ) |> 
  filter(!is.na(screener_decision)) |> 
  arrange(screener, final_human_decision) |>
  relocate(screener:screener_decision, .before = final_human_decision)

sport_single_perform_dat_2screen <- 
  sport_dat_2screen |> 
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
    review_authors = "Filges, Verner, et al. (2023)",
    review = "Organised Sport",
    role = c("Assistant", "Assistant", "Author")
  ) |> 
  relocate(review_authors:role)

saveRDS(sport_single_perform_dat_2screen, "single screener data/sport_single_perform_dat_2screen.rds")

