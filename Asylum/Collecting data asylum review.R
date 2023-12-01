library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)

html <- read_html("Asylum/Asylum screening level 1 full coding report.html")

screen_report_dat_raw <- 
  html |> 
  html_element("table") |> 
  html_table() |> 
  filter(str_detect(`I/E/D/S flag`, "(I)|(E)")) 

screen_report_dat_raw |> glimpse()

rm(html)

screen_report_dat <- 
  screen_report_dat_raw |> 
  select(-`I/E/D/S flag`) |> 
  rename(
    eppi_id= ItemId,
    author_short = ShortTitle, 
    title_report = Title, 
    exclude1 = `Exclude on topic`, 
    exclude2 = `Exclude on participants`,
    exclude3 = `Exclude on study design`,
    include = `Included on title and abstract`
  ) |> 
  mutate(
    across(exclude1:include, ~ str_remove_all(.x, "Ã¡ "))
  ) |> 
  arrange(eppi_id) 

rm(screen_report_dat_raw)

# Loading included and excluded studies
ex_paths <- list.files("Asylum/", pattern = "excl")

asylum_excl <- 
  map(ex_paths, ~ {
    revtools::read_bibliography(paste0("Asylum/", .x)) |> 
      suppressWarnings() |> 
      as_tibble() |>
      select(author, eppi_id, title, abstract) |> # Using only relevant variables
      mutate(
        final_human_decision = 0
      )
  }
  ) |> 
  list_rbind()

asylum_incl <- 
  revtools::read_bibliography("Asylum/asylum_incl.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 1
  )

asylum_ris_dat <- bind_rows(asylum_excl, asylum_incl) 

asylum_dat_wide <- 
  left_join(screen_report_dat, asylum_ris_dat, by = join_by(eppi_id)) |> 
  arrange(final_human_decision)

rm(asylum_excl, asylum_incl)

# Detecting individual screener names
screeners_var <- 
  screen_report_dat |> 
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
    asylum_dat_wide |> 
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
    exclude = if_any(exclude1:exclude3, ~ !is.na(.x)),
    exclude = if_else(exclude == TRUE, screener, NA_character_),
    screener_decision = case_when(
      !is.na(include) ~ 1,
      !is.na(exclude) ~ 0,
      !is.na(include) & !is.na(exclude) ~ 2, # Indicating non decisive answer
      TRUE ~ NA_real_
    ),
    conflict = if_else(screener_decision != final_human_decision, 1, 0)
  ) |> 
  select(-c(exclude1:exclude3)) |> 
  relocate(exclude, .before = include) |> 
  relocate(screener_decision, .before = final_human_decision)

rm(filter_list)

#Check 
sum(single_screen_dat$screener_decision == 2)


asylum_single_perform_dat <- 
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
    review_authors = "Filges, Montgomery et al. (2015)",
    review = "Asylum",
    role = rep(c("Assistant", "Assistant", "Author"), 1),
  ) |> 
  relocate(review_authors:role)

saveRDS(asylum_single_perform_dat, "single screener data/All screenings/asylum_single_perform_dat.rds")

# Extracting all individual screener scores in wide format to exclude training references

single_screen_dat_wide <- 
  single_screen_dat |> 
  pivot_wider(
    id_cols = eppi_id,
    id_expand = TRUE,
    values_from = screener_decision,
    names_from = screener,
  )

asylum_dat <- 
  left_join(asylum_dat_wide, single_screen_dat_wide, by = join_by(eppi_id)) |> 
  relocate(final_human_decision, .after = last_col()) |> 
  rowwise() |> 
  mutate(
    n_screeners = sum(!is.na(c_across(`Therese Friis`:`Trine Filges`)))
  ) |> 
  ungroup()

n_refs <- asylum_dat |> filter(n_screeners == 2) |> nrow()
saveRDS(n_refs, "single screener data/Number of References/asylum_n_refs.rds")

cor_dat <- 
  asylum_dat |> 
  filter(n_screeners == 2) |> 
  select(`Therese Friis`:`Malan Dunga`) |> 
  rename(screener1 = `Therese Friis`, screener2 = `Malan Dunga`) |> 
  mutate(
    across(everything(), ~ as.integer(.x))
  )

cor_mat <- cor(cor_dat) |> as.data.frame()
saveRDS(cor_mat, "single screener data/Between screener correlation/asylum_cor_mat.rds")  


asylum_dat_2screen <- 
  asylum_dat |> 
  # Removing train data plus uncertainty decisions
  filter(n_screeners == 2) |> 
  select(-n_screeners) |> 
  pivot_longer(
    cols = `Therese Friis`:`Trine Filges`,
    names_to = "screener",
    values_to = "screener_decision"
  ) |> 
  filter(!is.na(screener_decision)) |> 
  arrange(screener, final_human_decision) |>
  relocate(screener:screener_decision, .before = final_human_decision)

asylum_single_perform_dat_2screen <- 
  asylum_dat_2screen |> 
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
    review_authors = "Filges, Montgomery et al. (2015)",
    review = "Asylum",
    role = rep(c("Assistant"), 2),
  ) |> 
  relocate(review_authors:role)

saveRDS(asylum_single_perform_dat_2screen, "single screener data/asylum_single_perform_dat_2screen.rds")

#------------------------------------------------------------------------------
# Old
#------------------------------------------------------------------------------

#asylum_dat <- 
#  left_join(screen_report_dat, asylum_ris_dat, by = join_by(eppi_id)) |> 
#  select(-c(author_short, title_report)) |> 
#  relocate(exclude:include, .before = final_human_decision) |> 
#  arrange(final_human_decision) |> 
#  mutate(
#    review_authors = "Filges, Montgomery et al. (2015)",
#    review = "Asylum",
#    studyid = 1:n(),
#    abstract = str_remove_all(abstract, "\\<bold\\>"),
#    conflict = if_else(!is.na(exclude) & !is.na(include), 1, 0)
#  ) |> 
#  relocate(studyid, .after = eppi_id) |> 
#  relocate(author) |> 
#  relocate(review_authors:review)
#
## Level of conflicts
#asylum_dat |> 
#  summarise(
#    number_of_references = n(),
#    number_of_conflicts = sum(conflict == 1), 
#    percent_conflict = mean(conflict == 1),
#    .by = final_human_decision
#  )
#
#saveRDS(asylum_dat, file = "all data sets/asylum_dat.rds")
#