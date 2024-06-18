# Create test data for TF

library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)

html <- read_html("TF/Screening_ Titel_Abstract full coding report.html")

code_report_dat <- 
  html |> 
  html_element("table") |> 
  html_table()

code_rep_dat <- 
  code_report_dat |> 
  select(-`I/E/D/S flag`) |> 
  rename(
    eppi_id= ItemId,
    author_short = ShortTitle, 
    title_report = Title, 
    include = Include, 
    exclude = Exclude
  ) |> 
  mutate(
    across(include:exclude, ~ na_if(.x, ""))
  ) |> 
  arrange(eppi_id) |> 
  filter(eppi_id != "FullPath:") 


# Loading included and excluded studies

ex_paths <- list.files(path = "TF", pattern = "excl")

tf_excl <- 
  map(ex_paths, ~ {
    revtools::read_bibliography(paste0("TF/", .x)) |> 
      suppressWarnings() |> 
      as_tibble() |>
      select(author, eppi_id, title, abstract) |> # Using only relevant variables
      mutate(
        human_code = 0
      )
  }
  ) |> 
  list_rbind()

tf_incl <- revtools::read_bibliography("TF/TF included.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |>
  mutate(
    human_code = 1
  )

tf_raw_dat <- 
  bind_rows(tf_excl, tf_incl) |> # Removing one reference that appears among in and excluded references
  arrange(eppi_id)


# Removing the 4 references that does not appear among ris-file references
ids <- tf_raw_dat$eppi_id

code_rep_dat_filtered <- 
  code_rep_dat |> 
  filter(eppi_id %in% ids)  

tf_dat_full_wide <- 
  left_join(code_rep_dat_filtered, tf_raw_dat, by = join_by(eppi_id)) |> 
  filter(!is.na(abstract)) |> 
  arrange(human_code) |> 
  mutate(
    studyid = 1:n(),
    abstract = str_remove_all(abstract, "\\<bold\\>"),
    conflict = if_else(!is.na(exclude) & !is.na(include), 1, 0)
  ) |> 
  relocate(studyid, .after = eppi_id) |> 
  #select(-c(author_short:title_report)) |> 
  relocate(author, .before = eppi_id) |> 
  relocate(include:exclude, .before = human_code)

# Detecting individual screener names
screeners_var <- 
  code_rep_dat_filtered |> 
  reframe(
    name = unique(c_across(include:exclude))
  ) |> 
  pull(name)

screeners <- 
  unlist(strsplit(screeners_var, "(?<=[a-z])(?=[A-Z])", perl = TRUE)) |> 
  gsub("\\[.*","", x = _) |> 
  unique() 

screeners <- screeners[-1]


filter_list <- list()

for (i in 1:length(screeners)){
  filter_list[[i]] <- 
    tf_dat_full_wide  |> 
    filter(if_any(include:exclude, ~ str_detect(.x, screeners[i]))) |> 
    mutate(
      screener = screeners[i],
      across(include:exclude, ~ str_extract(.x, screeners[i]))
    ) |> 
    relocate(screener)
}

single_screen_dat <- 
  filter_list |> 
  list_rbind() |> 
  mutate(
    exclude = if_any(exclude, ~ !is.na(.x)),
    exclude = if_else(exclude == TRUE, screener, NA_character_),
    screener_decision = case_when(
      !is.na(include) ~ 1,
      !is.na(exclude) ~ 0,
      !is.na(include) & !is.na(exclude) ~ 2, # Indicating non decisive answer
      TRUE ~ NA_real_
    ),
    conflict = if_else(screener_decision != human_code, 1, 0)
  ) |> 
  relocate(exclude, .before = include) |> 
  relocate(screener_decision, .before = human_code)


#----------------------------------------------------------------------------------------
# Extracting all individual screener scores in wide format to exclude training references
# and not double-screened references 
#----------------------------------------------------------------------------------------

single_screen_dat_wide <- 
  single_screen_dat |> 
  pivot_wider(
    id_cols = eppi_id,
    id_expand = TRUE,
    values_from = screener_decision,
    names_from = screener,
  )

tf_dat <- 
  left_join(tf_dat_full_wide, single_screen_dat_wide, by = join_by(eppi_id)) |> 
  relocate(human_code, .after = last_col()) |> 
  rowwise() |> 
  mutate(
    n_screeners = sum(!is.na(c_across(`Tanne Ebert JÃ¸rgensen`:`Frederikke Schytt`)))
  ) |> 
  ungroup() |> 
  # Removing Jens Dietrichson since his main role has been to act as a 
  # the third screening mediator deciding whether studies should be included or not
  filter(is.na(`Jens Dietrichson`)) |> 
  select(-c(`Jens Dietrichson`))

#saveRDS(tf_dat, "tf_dat_full.rds")

n_refs <- tf_dat |> filter(n_screeners == 2) |> nrow()
saveRDS(n_refs, "single screener data/Number of References/tf_n_refs.rds")

n_in <- tf_dat |> filter(n_screeners == 2 & human_code == 1) |> nrow()
saveRDS(n_in, "single screener data/Number of References/tf_n_in.rds")

tf_dat_2screen <- 
  tf_dat |> 
  filter(n_screeners == 2) |> 
  select(-n_screeners) |> 
  pivot_longer(
    cols = `Tanne Ebert JÃ¸rgensen`:`Frederikke Schytt`,
    names_to = "screener",
    values_to = "screener_decision"
  ) |> 
  filter(!is.na(screener_decision)) |> 
  arrange(screener, human_code) |>
  relocate(screener:screener_decision, .before = human_code)

tf_single_perform_dat_2screen <- 
  tf_dat_2screen |> 
  summarise(
    TP = sum(screener_decision == 1 & human_code == 1, na.rm = TRUE),
    TN = sum(screener_decision == 0 & human_code == 0, na.rm = TRUE),
    FN = sum(screener_decision == 0 & human_code == 1, na.rm = TRUE),
    FP = sum(screener_decision == 1 & human_code == 0, na.rm = TRUE),
    recall = TP / (TP + FN),
    spec = TN / (TN + FP),
    bacc = (recall + spec) / 2,
    .by = screener
  ) |> 
  ungroup() |> 
  mutate(
    review_authors = "Thomsen et al. (2022)",
    review = "Testing Frequency",
    role = rep(c("Author", "Assistant", "Author", "Assistant"), c(1,4,1,1))
  ) |> 
  relocate(review_authors:role)

saveRDS(tf_single_perform_dat_2screen, "single screener data/tf_single_perform_dat_2screen.rds")

