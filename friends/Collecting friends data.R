library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)

html <- read_html("FRIENDS Title & Abstract full coding report.html")

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
    exclude = EXCLUDE, 
    include = `INCLUDE on T/A`
  ) |> 
  mutate(
    across(exclude:include, ~ na_if(.x, ""))
  ) |> 
  arrange(eppi_id) |> 
  filter(eppi_id != "FullPath:") # removing first row

# Removing fullpath row
#code_rep_dat <- code_rep_dat[-1,]


# Loading included and excluded studies
friends_excl <- revtools::read_bibliography("friends_excl.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    human_code = 0
  )

friends_incl <- revtools::read_bibliography("friends_incl.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |>
  mutate(
    human_code = 1
  )

friends_raw_dat <- 
  bind_rows(filter(friends_excl, eppi_id != "91822596"), friends_incl) |> # Removing one reference that appears among in and excluded references
  arrange(eppi_id)
  

# Remove gray lit refs
html2 <- read_html("gray search.html")

gray_ids <- 
  html2 |> 
  html_element("table") |> 
  html_table() |> 
  select(ID) |> 
  pull(ID) |> 
  as.character()

friends_dat <- 
  left_join(code_rep_dat, friends_raw_dat, by = join_by(eppi_id)) |> 
  filter(!eppi_id %in% gray_ids) |> 
  filter(!is.na(abstract)) |> 
  arrange(human_code) |> 
  mutate(
    studyid = 1:n(),
    abstract = str_remove_all(abstract, "\\<bold\\>"),
    conflict = if_else(!is.na(exclude) & !is.na(include), 1, 0)
  ) |> 
  relocate(studyid, .after = eppi_id) |> 
  select(-c(author_short:title_report)) |> 
  relocate(author, .before = eppi_id) |> 
  relocate(exclude:include, .before = human_code)

saveRDS(friends_dat, "friends_dat.rds")

# Level of conflicts
friends_dat |> 
  summarise(
    number_of_references = n(),
    number_of_conflicts = sum(conflict == 1), 
    percent_conflict = mean(conflict == 1),
    .by = human_code
  )


# Setting seed
set.seed(30102023)

# Helper function
sample_ref <- function(dat, x, with_replacement = FALSE, prob_vec = rep(1/x, nrow(dat))) {
  dat[sample(NROW(dat), size = x, replace = with_replacement, prob = prob_vec),]
}

excl <- sample_ref(filter(friends_dat, human_code == 0), 150)
incl <- sample_ref(filter(friends_dat, human_code == 1), 50)

# for myself
friends_test_dat <- 
  bind_rows(excl, incl) |> 
  mutate(
    across(title:abstract, ~ base::gsub("<.*?>", "", .x))
  )

# for julian
friends_test_dat <- 
  bind_rows(excl, incl) |> 
  sample_ref(200) 

# Save data
#write_xlsx(friends_test_dat, "friends_test_dat_mikkel.xlsx")
saveRDS(friends_test_dat, "friends_test_dat_mikkel.rds")

