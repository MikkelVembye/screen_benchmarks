# Creating data for outreach
library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)
library(readr)

html <- read_html("Outreach/Outreach screen on title & abstract full coding report.html")

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
    exclude1 = `EXCLUDE`, 
    exclude2 = `EXCLUDE Review`,
    include = `INCLUDE on title & abstract`
  ) |> 
  arrange(eppi_id)

rm(screen_report_dat_raw)

# Loading included and excluded studies
ex_paths <- list.files("Outreach/", pattern = "excl")

outreach_excl <- 
  map(ex_paths, ~ {
    revtools::read_bibliography(paste0("Outreach/", .x)) |> 
      suppressWarnings() |> 
      as_tibble() |>
      select(author, eppi_id, title, abstract) |> # Using only relevant variables
      mutate(
        final_human_decision = 0
      )
  }
  ) |> 
  list_rbind()

outreach_incl <- revtools::read_bibliography("Outreach/outreach_include.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |>
  mutate(
    final_human_decision = 1
  )

outreach_ris_dat <- bind_rows(outreach_excl, outreach_incl) 

rm(outreach_excl)
rm(outreach_incl)

outreach_dat_wide <- 
  left_join(screen_report_dat, outreach_ris_dat, by = join_by(eppi_id)) |> 
  arrange(final_human_decision) 

# Individual screener names
screeners <- c("Frederikke Blohm", "Maria Chabala Tilsted Mumba", "Frederikke Schytt",
               "Trine  Piepgras Carstens", "Trine Filges", "Nina Thorup Dalgaard")

filter_list <- list()

for (i in 1:length(screeners)){
  filter_list[[i]] <- 
    outreach_dat_wide |> 
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
    #exclude = if_else(!is.na(exclude), screener, NA_character_),
    exclude = if_any(exclude1:include, ~ !is.na(.x)),
    exclude = if_else(exclude == TRUE, screener, NA_character_),
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
outreach_single_perform_dat <- 
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
    review_authors = "Filges, Dalgaard, & Viinholt (2022)",
    review = "Outreach",
    role = rep(c("Assistant", "Author"), c(4,2))
  ) |> 
  relocate(review_authors:role) 

saveRDS(outreach_single_perform_dat, "single screener data/All screenings/outreach_single_perform_dat.rds")

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

outreach_dat <- 
  left_join(outreach_dat_wide, single_screen_dat_wide, by = join_by(eppi_id)) |> 
  relocate(final_human_decision, .after = last_col()) |> 
  rowwise() |> 
  mutate(
    n_screeners = sum(!is.na(c_across(`Frederikke Blohm`:`Nina Thorup Dalgaard`)))
  ) |> 
  ungroup()

n_refs <- outreach_dat |> filter(n_screeners == 2) |> nrow()
saveRDS(n_refs, "single screener data/Number of References/outreach_n_refs.rds")

n_in <- outreach_dat |> filter(n_screeners == 2 & final_human_decision == 1) |> nrow()
saveRDS(n_in, "single screener data/Number of References/outreach_n_in.rds")


outreach_dat_2screen <- 
  outreach_dat |> 
  filter(n_screeners == 2) |>
  select(-n_screeners) |> 
  pivot_longer(
    cols = `Frederikke Blohm`:`Nina Thorup Dalgaard`,
    names_to = "screener",
    values_to = "screener_decision"
  ) |> 
  filter(!is.na(screener_decision)) |> 
  arrange(screener, final_human_decision) |>
  relocate(screener:screener_decision, .before = final_human_decision)

outreach_single_perform_dat_2screen <- 
  outreach_dat_2screen |> 
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
    review_authors = "Filges, Dalgaard, & Viinholt (2022)",
    review = "Outreach",
    role ="Assistant"
  ) |> 
  relocate(review_authors:role)

saveRDS(outreach_single_perform_dat_2screen, "single screener data/outreach_single_perform_dat_2screen.rds")

