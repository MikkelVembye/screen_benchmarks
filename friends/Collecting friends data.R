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
    include = `INCLUDE on T/A`
  ) |> 
  arrange(eppi_id)


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

friends_ris_dat <- 
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

friends_dat_wide <- 
  left_join(screen_report_dat, friends_ris_dat, by = join_by(eppi_id)) |> 
  filter(!eppi_id %in% gray_ids) |> 
  #filter(!is.na(abstract)) |> 
  arrange(final_human_decision) 

n_refs <- friends_dat_wide |> nrow()
saveRDS(n_refs, "single screener data/Number of References/friends_n_refs.rds")

n_in <- friends_dat_wide |> filter(final_human_decision == 1) |> nrow()
saveRDS(n_in, "single screener data/Number of References/friends_n_in.rds")

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

screeners <- screeners[1:2]

filter_list <- list()

for (i in 1:length(screeners)){
  filter_list[[i]] <- 
    friends_dat_wide |> 
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


friends_single_perform_dat <- 
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
    review_authors = "Filges et al. (forthcoming)",
    review = "FRIENDS",
    role = c("Author", "Assistant"),
  ) |> 
  relocate(review_authors:role) 

saveRDS(friends_single_perform_dat, "single screener data/friends_single_perform_dat_2screen.rds")


#------------------------------------------------------------------------------
# Old
#------------------------------------------------------------------------------

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



