
library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)
library(ggplot2)
library(purrr)
library(tidyr)
library(metafor)
library(tidytext)

options(pillar.sigfig = 4) # ensure tibble include 4 digits
#options(tibble.width = Inf)
#options(dplyr.print_min = 20)
options(scipen = 10)
options(dplyr.summarise.inform = FALSE) # Avoids summarize info from tidyverse

################################################################################
# 1. Loading data for (Jadim et al. 2021)
################################################################################

html_first_epi <- read_html("NIPH/First episode psychosis/Screen on Title & Abstract full coding report_first episode psychosis.html")

screen_report_raw_first_epi <- 
  html_first_epi |> 
  html_element("table") |> 
  html_table() |> 
  filter(str_detect(`I/E/D/S flag`, "(I)|(E)")) 

screen_report_raw_first_epi |> glimpse()

screen_report_first_epi <- 
  screen_report_raw_first_epi |> 
  select(-`I/E/D/S flag`) |> 
  rename(
    eppi_id= ItemId,
    author_short = ShortTitle, 
    title_report = Title, 
    exclude1 = Eksluder, 
    exclude2 = `Ekskludert, men Relevant litteratur`,
    include1 = `Inkluder SR`,
    include2 = `Inkluder RCT`
  ) 

first_epi_excl <- 
  revtools::read_bibliography("NIPH/First episode psychosis/Excluded_T&A_First_Episode_Psychosis_direct_from_EPPI.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 0
  )

first_epi_incl <- 
  revtools::read_bibliography("NIPH/First episode psychosis/Included_T&A_First_Episode_Psychosis_direct_from_EPPI.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 1
  )

first_epi_ris_dat <- bind_rows(first_epi_incl, first_epi_excl) 

# Removing the 12 references that does not appear among ris-file references
ids_first_epi <- first_epi_ris_dat$eppi_id

screen_report_first_epi_filtered <- 
  screen_report_first_epi |> 
  filter(eppi_id %in% ids_first_epi)

# Binding screen report and ris-file data together
first_epi_dat_wide <- 
  left_join(screen_report_first_epi_filtered, first_epi_ris_dat, by = join_by(eppi_id)) |> 
  arrange(final_human_decision)

# Detecting individual screeners names
screeners_var_first_epi <- 
  screen_report_first_epi_filtered |> 
  reframe(
    name = unique(c_across(exclude1:include2))
  ) |> 
  pull(name)

screeners_first_epi <- 
  unlist(strsplit(screeners_var_first_epi, "(?<=[a-z])(?=[A-Z])", perl = TRUE)) |> 
  gsub("\\[.*","", x = _) |> 
  unique() 

# Making long format data
filter_list <- list()

for (i in 1:length(screeners_first_epi)){
  filter_list[[i]] <- 
    first_epi_dat_wide |> 
    filter(if_any(exclude1:include2, ~ str_detect(.x, screeners_first_epi[i]))) |> 
    mutate(
      screener = screeners_first_epi[i],
      across(exclude1:include2, ~ str_extract(.x, screeners_first_epi[i]))
    ) |> 
    relocate(screener)
}

single_screen_dat_first_epi <- 
  filter_list |> 
  list_rbind() |> 
  mutate(
    exclude = if_any(exclude1:exclude2, ~ !is.na(.x)),
    exclude = if_else(exclude == TRUE, screener, NA_character_),
    include = if_any(include1:include2, ~ !is.na(.x)),
    include = if_else(include == TRUE, screener, NA_character_),
    screener_decision = case_when(
      !is.na(include) ~ 1,
      !is.na(exclude) ~ 0,
      !is.na(include) & !is.na(exclude) ~ 2, # Indicating non decisive answer
      TRUE ~ NA_real_
    ),
    conflict = if_else(screener_decision != final_human_decision, 1, 0)
  ) |> 
  select(-c(exclude1:include2)) |> 
  relocate(exclude, .before = include) |> 
  relocate(screener_decision, .before = final_human_decision)


single_screen_dat_first_epi_wide <- 
  single_screen_dat_first_epi |> 
  pivot_wider(
    id_cols = eppi_id,
    id_expand = TRUE,
    values_from = screener_decision,
    names_from = screener,
  )

first_epi_dat <- 
  left_join(first_epi_dat_wide, single_screen_dat_first_epi_wide, by = join_by(eppi_id)) |> 
  relocate(final_human_decision, .after = last_col()) |> 
  rowwise() |> 
  mutate(
    n_screeners = sum(!is.na(c_across(`Trine Bjerke Johansen`:`Patricia Sofia Jacobsen Jardim`)))
  ) |> 
  ungroup()

n_refs <- first_epi_dat |> filter(n_screeners == 2) |> nrow()
saveRDS(n_refs, "NIPH/Data/n references/first_epi_n_refs.rds")

n_in <- first_epi_dat |> filter(n_screeners == 2 & final_human_decision == 1) |> nrow()
saveRDS(n_in, "NIPH/Data/n references/first_epi_n_in.rds")

first_epi_dat_2screen <- 
  first_epi_dat |> 
  # Removing train data plus single screened references
  filter(n_screeners == 2) |> 
  select(-n_screeners) |> 
  pivot_longer(
    cols = `Trine Bjerke Johansen`:`Patricia Sofia Jacobsen Jardim`,
    names_to = "screener",
    values_to = "screener_decision"
  ) |> 
  filter(!is.na(screener_decision)) |> 
  arrange(screener, final_human_decision) |>
  relocate(screener:screener_decision, .before = final_human_decision)

first_epi_single_perform_dat_2screen <- 
  first_epi_dat_2screen |> 
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
    review_authors = "Jadim et al. 2021",
    review = "First episode psychosis"
  ) |> 
  relocate(review_authors)

saveRDS(first_epi_single_perform_dat_2screen, "NIPH/Data/first_epi_single_perform_dat_2screen.rds")

rm(list = ls())


################################################################################
# 2. Loading data for (Johansen et al., 2022)
################################################################################

html_joint <- read_html("NIPH/Joint custody/Screen on Title & Abstract full coding report_Joint Custody.html")

screen_report_raw_joint <- 
  html_joint |> 
  html_element("table") |> 
  html_table() |> 
  filter(str_detect(`I/E/D/S flag`, "(I)|(E)")) 

screen_report_raw_joint |> glimpse()

screen_report_joint <- 
  screen_report_raw_joint |> 
  select(-`I/E/D/S flag`) |> 
  rename(
    eppi_id = ItemId,
    author_short = ShortTitle, 
    title_report = Title, 
    include = `INCLUDE on title & abstract`,
    exclude1 = EXCLUDE, 
    exclude2 = `Exclude, but interesting`,
    exclude3 = `Put on exclude list` 
  ) |> 
  mutate(
    across(include:exclude3, ~ str_replace_all(.x, "Ã¸", "oe"))
  ) |> 
  arrange(eppi_id)

joint_excl <- 
  revtools::read_bibliography("NIPH/Joint custody/Excludes T&A _joint custody.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 0
  )

joint_incl <- 
  revtools::read_bibliography("NIPH/Joint custody/Includes T&A _joint custody.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 1
  )

joint_ris_dat <- bind_rows(joint_incl, joint_excl) 

# Removing the 60 references that does not appear among ris-file references
ids_joint <- joint_ris_dat$eppi_id

screen_report_joint_filtered <- 
  screen_report_joint |> 
  filter(eppi_id %in% ids_joint)

# Binding screen report and ris-file data together
joint_dat_wide <- 
  left_join(screen_report_joint_filtered, joint_ris_dat, by = join_by(eppi_id)) |> 
  arrange(final_human_decision)

# Detecting individual screeners names
screeners_var_joint <- 
  screen_report_joint_filtered |> 
  reframe(
    name = unique(c_across(include:exclude3))
  ) |> 
  pull(name)

screeners_joint <- 
  unlist(strsplit(screeners_var_joint, "(?<=[a-z])(?=[A-Z])", perl = TRUE)) |> 
  gsub("\\[.*","", x = _) |> 
  unique() 

# Making long format data
filter_list <- list()

for (i in 1:length(screeners_joint)){
  filter_list[[i]] <- 
    joint_dat_wide |> 
    filter(if_any(include:exclude3, ~ str_detect(.x, screeners_joint[i]))) |> 
    mutate(
      screener = screeners_joint[i],
      across(include:exclude3, ~ str_extract(.x, screeners_joint[i]))
    ) |> 
    relocate(screener)
}

single_screen_dat_joint <- 
  filter_list |> 
  list_rbind() |> 
  mutate(
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


single_screen_dat_joint_wide <- 
  single_screen_dat_joint |> 
  pivot_wider(
    id_cols = eppi_id,
    id_expand = TRUE,
    values_from = screener_decision,
    names_from = screener,
  )

joint_dat <- 
  left_join(joint_dat_wide, single_screen_dat_joint_wide, by = join_by(eppi_id)) |> 
  relocate(final_human_decision, .after = last_col()) |> 
  rowwise() |> 
  mutate(
    n_screeners = sum(!is.na(c_across(`Heid Noekleby`:`Tiril Borge`)))
  ) |> 
  ungroup()

n_refs <- joint_dat |> filter(n_screeners == 2) |> nrow()
saveRDS(n_refs, "NIPH/Data/n references/joint_n_refs.rds")

n_in <- joint_dat |> filter(n_screeners == 2 & final_human_decision == 1) |> nrow()
saveRDS(n_in, "NIPH/Data/n references/joint_n_in.rds")

joint_dat_2screen <- 
  joint_dat |> 
  # Removing train data plus single screened references
  filter(n_screeners == 2) |> 
  select(-n_screeners) |> 
  pivot_longer(
    cols = `Heid Noekleby`:`Tiril Borge`,
    names_to = "screener",
    values_to = "screener_decision"
  ) |> 
  filter(!is.na(screener_decision)) |> 
  arrange(screener, final_human_decision) |>
  relocate(screener:screener_decision, .before = final_human_decision)

joint_single_perform_dat_2screen <- 
  joint_dat_2screen |> 
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
    review_authors = "Johansen et al. 2022",
    review = "Joint_custody"
  ) |> 
  relocate(review_authors)

saveRDS(joint_single_perform_dat_2screen, "NIPH/Data/joint_single_perform_dat_2screen.rds")

rm(list = ls())

################################################################################
# 3. Loading data for (Meneses-Echavez et al., 2022)
################################################################################

html_psyc_deb <- read_html("NIPH/Psychological debriefing/Screen on Title & Abstract full coding report_psychological debriefing.html")

screen_report_raw_psyc_deb <- 
  html_psyc_deb |> 
  html_element("table") |> 
  html_table() |> 
  filter(str_detect(`I/E/D/S flag`, "(I)|(E)")) 

screen_report_raw_psyc_deb |> glimpse()

screen_report_psyc_deb <- 
  screen_report_raw_psyc_deb |> 
  select(-`I/E/D/S flag`) |> 
  rename(
    eppi_id = ItemId,
    author_short = ShortTitle, 
    title_report = Title, 
    include1 = `INCLUDE on title & abstract`,
    exclude = EXCLUDE,
    include2 = `Relevant litteratur`,
    include3 = `Include for second opinion`,
  ) |> 
  mutate(
    across(include1:include3, ~ str_replace_all(.x, "Ã¥", "ae"))
  ) |> 
  arrange(eppi_id) # |> 
  # keep included and excluded studies only
  #filter(if_all(other:uncertain, ~ .x == "")) |> 
  #select(-c(other:uncertain))

# Problem here
psyc_deb_excl <- 
  revtools::read_bibliography("NIPH/Psychological debriefing/ExcludesT&A_Psychologicaldebriefing_n8179.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 0
  )

psyc_deb_incl <- 
  revtools::read_bibliography("NIPH/Psychological debriefing/IncludesT&A_Psychologicaldebriefing_n57.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 1
  )

psyc_deb_ris_dat <- bind_rows(psyc_deb_incl, psyc_deb_excl) 

# # There was 12 references that were both included and excluded
ids_psyc_deb_double <- 
  psyc_deb_ris_dat |> 
  mutate(n = n(), .by = eppi_id) |> 
  filter(n > 1 & final_human_decision == 0) |> 
  pull(eppi_id) |> 
  unique()

psyc_deb_excl_filtered <- 
  psyc_deb_excl |> 
  filter(!eppi_id %in% ids_psyc_deb_double)

psyc_deb_ris_dat_filtered <- bind_rows(psyc_deb_incl, psyc_deb_excl_filtered)

ids_psyc_deb <- psyc_deb_ris_dat_filtered$eppi_id 

screen_report_psyc_deb_filtered <- 
  screen_report_psyc_deb |> 
  filter(eppi_id %in% ids_psyc_deb)


# Binding screen report and ris-file data together
psyc_deb_dat_wide <- 
  left_join(screen_report_psyc_deb_filtered, psyc_deb_ris_dat_filtered, by = join_by(eppi_id)) |> 
  arrange(final_human_decision)

# Detecting individual screeners names
screeners_var_psyc_deb <- 
  screen_report_psyc_deb_filtered |> 
  reframe(
    name = unique(c_across(include1:include3))
  ) |> 
  pull(name)

screeners_psyc_deb <- 
  unlist(strsplit(screeners_var_psyc_deb, "(?<=[a-z])(?=[A-Z])", perl = TRUE)) |> 
  gsub("\\[.*","", x = _) |> 
  unique()

screeners_psyc_deb <- screeners_psyc_deb[1:4]

# Making long format data
filter_list <- list()

for (i in 1:length(screeners_psyc_deb)){
  filter_list[[i]] <- 
    psyc_deb_dat_wide |> 
    filter(if_any(include1:include3, ~ str_detect(.x, screeners_psyc_deb[i]))) |> 
    mutate(
      screener = screeners_psyc_deb[i],
      across(include1:include3, ~ str_extract(.x, screeners_psyc_deb[i]))
    ) |> 
    relocate(screener)
}

single_screen_dat_psyc_deb <- 
  filter_list |> 
  list_rbind() |> 
  mutate(
    exclude = if_else(!is.na(exclude), screener, NA_character_),
    include = if_any(c(include1, include2, include3), ~ !is.na(.x)),
    include = if_else(include == TRUE, screener, NA_character_),
    screener_decision = case_when(
      !is.na(include) ~ 1,
      !is.na(exclude) ~ 0,
      !is.na(include) & !is.na(exclude) ~ 2, # Indicating non decisive answer
      TRUE ~ NA_real_
    ),
    conflict = if_else(screener_decision != final_human_decision, 1, 0)
  ) |> 
  select(-c(include1, include2, include3)) |> 
  relocate(exclude, .before = include) |> 
  relocate(screener_decision, .before = final_human_decision)


single_screen_dat_psyc_deb_wide <- 
  single_screen_dat_psyc_deb |> 
  pivot_wider(
    id_cols = eppi_id,
    id_expand = TRUE,
    values_from = screener_decision,
    names_from = screener,
  )

psyc_deb_dat <- 
  left_join(psyc_deb_dat_wide, single_screen_dat_psyc_deb_wide, by = join_by(eppi_id)) |> 
  relocate(final_human_decision, .after = last_col()) |> 
  rowwise() |> 
  mutate(
    n_screeners = sum(!is.na(c_across(`Jon-Vidar Gaustad`:`Tiril Borge`)))
  ) |> 
  ungroup()

n_refs <- psyc_deb_dat |> filter(n_screeners == 2) |> nrow()
saveRDS(n_refs, "NIPH/Data/n references/psyc_deb_n_refs.rds")

n_in <- psyc_deb_dat |> filter(n_screeners == 2 & final_human_decision == 1) |> nrow()
saveRDS(n_in, "NIPH/Data/n references/psyc_deb_n_in.rds")

psyc_deb_dat_2screen <- 
  psyc_deb_dat |> 
  # Removing train data plus single screened references
  filter(n_screeners == 2) |> 
  select(-n_screeners) |> 
  pivot_longer(
    cols = `Jon-Vidar Gaustad`:`Tiril Borge`,
    names_to = "screener",
    values_to = "screener_decision"
  ) |> 
  filter(!is.na(screener_decision)) |> 
  arrange(screener, final_human_decision) |>
  relocate(screener:screener_decision, .before = final_human_decision)

psyc_deb_single_perform_dat_2screen <- 
  psyc_deb_dat_2screen |> 
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
    review_authors = "Meneses-Echavez et al. 2022",
    review = "Psychological_debriefing"
  ) |> 
  relocate(review_authors)

saveRDS(psyc_deb_single_perform_dat_2screen, "NIPH/Data/psyc_deb_single_perform_dat_2screen.rds")

rm(list = ls())

################################################################################
# 4. Loading data for (Evensen et al., 2023)
################################################################################

html_rotator <- read_html("NIPH/Rotator cuff tears/Screen on Title & Abstract full coding report_rotator cuff tears.html")

screen_report_raw_rotator <- 
  html_rotator |> 
  html_element("table") |> 
  html_table() |> 
  filter(str_detect(`I/E/D/S flag`, "(I)|(E)")) 

screen_report_raw_rotator |> glimpse()

screen_report_rotator <- 
  screen_report_raw_rotator |> 
  select(-`I/E/D/S flag`) |> 
  rename(
    eppi_id = ItemId,
    author_short = ShortTitle, 
    title_report = Title, 
    exclude = EXCLUDE,
    include = `INCLUDE on title & abstract`
    
  ) |> 
  arrange(eppi_id)

rotator_excl <- 
  revtools::read_bibliography("NIPH/Rotator cuff tears/ExcludesT&A-rotator cuff.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 0
  )

rotator_incl <- 
  revtools::read_bibliography("NIPH/Rotator cuff tears/IncludesT&A-rotator cuff.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 1
  )

rotator_ris_dat <- bind_rows(rotator_incl, rotator_excl) 

ids_rotator <- rotator_ris_dat$eppi_id

screen_report_rotator_filtered <- 
  screen_report_rotator |> 
  filter(eppi_id %in% ids_rotator)

# Binding screen report and ris-file data together
rotator_dat_wide <- 
  left_join(screen_report_rotator_filtered, rotator_ris_dat, by = join_by(eppi_id)) |> 
  arrange(final_human_decision)

# Detecting individual screeners names
screeners_var_rotator <- 
  screen_report_rotator_filtered |> 
  reframe(
    name = unique(c_across(exclude:include))
  ) |> 
  pull(name)


screeners_rotator <- 
  unlist(strsplit(screeners_var_rotator, "(?<=[a-z])(?=[A-Z])", perl = TRUE)) |> 
  gsub("\\[.*","", x = _) |> 
  unique() 

# Making long format data
filter_list <- list()

for (i in 1:length(screeners_rotator)){
  filter_list[[i]] <- 
    rotator_dat_wide |> 
    filter(if_any(exclude:include, ~ str_detect(.x, screeners_rotator[i]))) |> 
    mutate(
      screener = screeners_rotator[i],
      across(exclude:include, ~ str_extract(.x, screeners_rotator[i]))
    ) |> 
    relocate(screener)
}

single_screen_dat_rotator <- 
  filter_list |> 
  list_rbind() |> 
  mutate(
    #exclude = if_any(exclude1:exclude3, ~ !is.na(.x)),
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
  relocate(screener_decision, .before = final_human_decision) |> 
  #remove the one and only reference screened by Tiril Borge 
  filter(screener != "Tiril Borge")

single_screen_dat_rotator_wide <- 
  single_screen_dat_rotator |> 
  pivot_wider(
    id_cols = eppi_id,
    id_expand = TRUE,
    values_from = screener_decision,
    names_from = screener,
  )

rotator_dat <- 
  left_join(rotator_dat_wide, single_screen_dat_rotator_wide, by = join_by(eppi_id)) |> 
  relocate(final_human_decision, .after = last_col()) |> 
  rowwise() |> 
  mutate(
    n_screeners = sum(!is.na(c_across(`Elisabet Hafstad`:`Kristin Thuve Dahm`)))
  ) |> 
  ungroup()

n_refs <- rotator_dat |> filter(n_screeners == 2) |> nrow()
saveRDS(n_refs, "NIPH/Data/n references/rotator_n_refs.rds")

n_in <- rotator_dat |> filter(n_screeners == 2 & final_human_decision == 1) |> nrow()
saveRDS(n_in, "NIPH/Data/n references/rotator_n_in.rds")

rotator_dat_2screen <- 
  rotator_dat |> 
  # Removing train data plus single screened references
  filter(n_screeners == 2) |> 
  select(-n_screeners) |> 
  pivot_longer(
    cols = `Elisabet Hafstad`:`Kristin Thuve Dahm`,
    names_to = "screener",
    values_to = "screener_decision"
  ) |> 
  filter(!is.na(screener_decision)) |> 
  arrange(screener, final_human_decision) |>
  relocate(screener:screener_decision, .before = final_human_decision)

rotator_single_perform_dat_2screen <- 
  rotator_dat_2screen |> 
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
    review_authors = "Evensen et al., 2023",
    review = "Rotator_cuff_tears"
  ) |> 
  relocate(review_authors)

saveRDS(rotator_single_perform_dat_2screen, "NIPH/Data/rotator_single_perform_dat_2screen.rds")

rm(list = ls())

################################################################################
# 5. Loading data for (Ames et al., 2022)
################################################################################

html_self <- read_html("NIPH/Self help apps/Screen on Title & Abstract full coding report_Self Help Apps.html")

screen_report_raw_self <- 
  html_self |> 
  html_element("table") |> 
  html_table() |> 
  filter(str_detect(`I/E/D/S flag`, "(I)|(E)")) 

screen_report_raw_self |> glimpse()

screen_report_self <- 
  screen_report_raw_self |> 
  select(-`I/E/D/S flag`) |> 
  rename(
    eppi_id = ItemId,
    author_short = ShortTitle, 
    title_report = Title, 
    exclude1 = `EXCLUDE on intervention`,
    exclude2 = `exclude for study design`,
    include1 = `INCLUDE for second opinion`,
    include2 = `INCLUDE on title & abstract`,
    include3 = Protocol
    
  ) |> 
  mutate(
    across(exclude1:include3, ~ str_replace_all(.x, "Ã¥", "ae"))
  ) |> 
  arrange(eppi_id)

# Problem here
self_excl <- 
  revtools::read_bibliography("NIPH/Self help apps/ExcludesT&A_Selfhelpapps_n7427.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 0
  )

self_incl <- 
  revtools::read_bibliography("NIPH/Self help apps/IncludesT&A_Selfhelpapps_n802.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 1
  )

self_ris_dat <- bind_rows(self_incl, self_excl) 

ids_self_dup <- 
  self_ris_dat |> 
  summarise(n = n(), .by = eppi_id) |> 
  filter(n > 1) |> 
  pull(eppi_id) |> 
  unique()

self_excl_filter <- 
  self_excl |> 
  filter(!eppi_id %in% ids_self_dup)

self_ris_dat_filter <- bind_rows(self_incl, self_excl_filter)

ids_self <- self_ris_dat_filter$eppi_id

screen_report_self_filtered <- 
  screen_report_self |> 
  filter(eppi_id %in% ids_self)

# Binding screen report and ris-file data together
self_dat_wide <- 
  left_join(screen_report_self_filtered, self_ris_dat_filter, by = join_by(eppi_id)) |> 
  arrange(final_human_decision)

# Detecting individual screeners names
screeners_var_self <- 
  screen_report_self_filtered |> 
  reframe(
    name = unique(c_across(exclude1:include3))
  ) |> 
  pull(name)

screeners_self <- 
  unlist(strsplit(screeners_var_self, "(?<=[a-z])(?=[A-Z])", perl = TRUE)) |> 
  gsub("\\[.*","", x = _) |> 
  unique() 

screeners_self <- screeners_self[1:3]

# Making long format data
filter_list <- list()

for (i in 1:length(screeners_self)){
  filter_list[[i]] <- 
    self_dat_wide |> 
    filter(if_any(exclude1:include3, ~ str_detect(.x, screeners_self[i]))) |> 
    mutate(
      screener = screeners_self[i],
      across(exclude1:include3, ~ str_extract(.x, screeners_self[i]))
    ) |> 
    relocate(screener)
}

single_screen_dat_self <- 
  filter_list |> 
  list_rbind() |> 
  mutate(
    exclude = if_any(exclude1:exclude2, ~ !is.na(.x)),
    exclude = if_else(exclude == TRUE, screener, NA_character_),
    include = if_any(include1:include3, ~ !is.na(.x)),
    include = if_else(include == TRUE, screener, NA_character_),
    screener_decision = case_when(
      !is.na(include) ~ 1,
      !is.na(exclude) ~ 0,
      !is.na(include) & !is.na(exclude) ~ 2, # Indicating non decisive answer
      TRUE ~ NA_real_
    ),
    conflict = if_else(screener_decision != final_human_decision, 1, 0)
  ) |> 
  select(-c(exclude1:include3)) |> 
  relocate(exclude, .before = include) |> 
  relocate(screener_decision, .before = final_human_decision) 

single_screen_dat_self_wide <- 
  single_screen_dat_self |> 
  pivot_wider(
    id_cols = eppi_id,
    id_expand = TRUE,
    values_from = screener_decision,
    names_from = screener,
  )

self_dat <- 
  left_join(self_dat_wide, single_screen_dat_self_wide, by = join_by(eppi_id)) |> 
  relocate(final_human_decision, .after = last_col()) |> 
  rowwise() |> 
  mutate(
    n_screeners = sum(!is.na(c_across(`Melanie Ames`:`Henriette Tyse Nygaerd`)))
  ) |> 
  ungroup()

n_refs <- self_dat |> filter(n_screeners == 2) |> nrow()
saveRDS(n_refs, "NIPH/Data/n references/self_n_refs.rds")

n_in <- self_dat |> filter(n_screeners == 2 & final_human_decision == 1) |> nrow()
saveRDS(n_in, "NIPH/Data/n references/self_n_in.rds")

self_dat_2screen <- 
  self_dat |> 
  # Removing train data plus single screened references
  filter(n_screeners == 2) |> 
  select(-n_screeners) |> 
  pivot_longer(
    cols = `Melanie Ames`:`Henriette Tyse Nygaerd`,
    names_to = "screener",
    values_to = "screener_decision"
  ) |> 
  filter(!is.na(screener_decision)) |> 
  arrange(screener, final_human_decision) |>
  relocate(screener:screener_decision, .before = final_human_decision)

self_single_perform_dat_2screen <- 
  self_dat_2screen |> 
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
    review_authors = "Ames et al., 2022",
    review = "Self_help_app"
  ) |> 
  relocate(review_authors)

saveRDS(self_single_perform_dat_2screen, "NIPH/Data/self_single_perform_dat_2screen.rds")

rm(list = ls())

################################################################################
# 6. Loading data for (Vist et al., 2022)
################################################################################

#html_women <- read_html("NIPH/Womens health/Screen on Title & Abstract full coding report_Womens Health.html")
#
#screen_report_raw_women <- 
#  html_women |> 
#  html_element("table") |> 
#  html_table() |> 
#  filter(str_detect(`I/E/D/S flag`, "(I)|(E)")) 
#
#screen_report_raw_women |> glimpse()
#
#screen_report_women <- 
#  screen_report_raw_women |> 
#  select(-`I/E/D/S flag`) |> 
#  rename(
#    eppi_id = ItemId,
#    author_short = ShortTitle, 
#    title_report = Title, 
#    exclude1 = EXCLUDE,
#    include = `INCLUDE on title & abstract`,
#    uncertain = `til diskusjon`,
#    exclude2 = `EXCLUDE LMIC or Norwegian studies not in PICO`,
#    exclude3 = `EXCLUDE kun sÃ¸kt i en database`,
#    exclude4 = `EXCLUDE Umbrella review eller oversikt over oversikter`,
#    exclude5 = `EXCLUDE men veldig interessant`
#  ) |> 
#  mutate(
#    across(exclude1:exclude5, ~ str_replace_all(.x, "BBahar Kucuk", "Bahar Kucuk")),
#    across(exclude1:exclude5, ~ str_replace_all(.x, "BLouise Forsetlund", "Louise Forsetlund")),
#    across(exclude1:exclude5, ~ str_replace_all(.x, "BChristine Hestevik", "Christine Hestevik"))
#  ) |> 
#  arrange(eppi_id) |> 
#  # removing uncertain references
#  filter(uncertain == "") |> 
#  select(-uncertain)
#
## Problem here
#women_excl <- 
#  revtools::read_bibliography("NIPH/Womens health/ExcludesT&A_Womenshealth_n16028.ris") |> 
#  suppressWarnings() |> 
#  as_tibble() |>
#  select(author, eppi_id, title, abstract) |> # Using only relevant variables
#  mutate(
#    final_human_decision = 0
#  )
#
#women_incl <- 
#  revtools::read_bibliography("NIPH/Womens health/IncludesT&A_Womenshealth_n4605.ris") |> 
#  suppressWarnings() |> 
#  as_tibble() |>
#  select(author, eppi_id, title, abstract) |> # Using only relevant variables
#  mutate(
#    final_human_decision = 1
#  )
#
#women_ris_dat <- bind_rows(women_incl, women_excl) 
#
## Find duplicate ids
#ids_women_dup <-  
#  women_ris_dat |> 
#  summarise(n = n(), .by = eppi_id) |> 
#  filter(n > 1) |> 
#  pull(eppi_id)
#
## Included reference (without uncertain decisions)
#ids_women_sr <- screen_report_women$eppi_id
#
#women_excl_filter <- 
#  women_excl |> 
#  filter(!eppi_id %in% ids_women_dup)
#
#women_incl_filter <- 
#  women_incl |> 
#  filter(!eppi_id %in% ids_women_dup)
#
#women_ris_dat_filtered <- 
#  bind_rows(women_incl_filter, women_excl_filter) |> 
#  filter(eppi_id %in% ids_women_sr)
#
#ids_women <- women_ris_dat_filtered$eppi_id
#
#screen_report_women_filtered <- 
#  screen_report_women |> 
#  filter(eppi_id %in% ids_women)
#
## Binding screen report and ris-file data together
#women_dat_wide <- 
#  left_join(screen_report_women_filtered, women_ris_dat, by = join_by(eppi_id)) |> 
#  arrange(final_human_decision)
#
## Detecting individual screeners names
#screeners_var_women <- 
#  screen_report_women_filtered |> 
#  reframe(
#    name = unique(c_across(exclude1:exclude5))
#  ) |> 
#  pull(name)
#
#screeners_women <- 
#  unlist(strsplit(screeners_var_women, "(?<=[a-z])(?=[A-Z])", perl = TRUE)) |> 
#  gsub("\\[.*","", x = _) |> 
#  unique() 
#  
#screeners_women <- screeners_women[1:6]
#
## Making long format data
#filter_list <- list()
#
#for (i in 1:length(screeners_women)){
#  filter_list[[i]] <- 
#    women_dat_wide |> 
#    filter(if_any(exclude1:exclude5, ~ str_detect(.x, screeners_women[i]))) |> 
#    mutate(
#      screener = screeners_women[i],
#      across(exclude1:exclude5, ~ str_extract(.x, screeners_women[i]))
#    ) |> 
#    relocate(screener)
#}
#
#single_screen_dat_women <- 
#  filter_list |> 
#  list_rbind() |> 
#  mutate(
#    exclude = if_any(c(exclude1, exclude2:exclude5), ~ !is.na(.x)),
#    exclude = if_else(exclude == TRUE, screener, NA_character_),
#    screener_decision = case_when(
#      !is.na(include) ~ 1,
#      !is.na(exclude) ~ 0,
#      !is.na(include) & !is.na(exclude) ~ 2, # Indicating non decisive answer
#      TRUE ~ NA_real_
#    ),
#    conflict = if_else(screener_decision != final_human_decision, 1, 0)
#  ) |> 
#  select(-c(exclude1, exclude2:exclude5)) |> 
#  relocate(exclude, .before = include) |> 
#  relocate(screener_decision, .before = final_human_decision) 
#
#single_screen_dat_women_wide <- 
#  single_screen_dat_women |> 
#  pivot_wider(
#    id_cols = eppi_id,
#    id_expand = TRUE,
#    values_from = screener_decision,
#    names_from = screener,
#  )
#
#women_dat <- 
#  left_join(women_dat_wide, single_screen_dat_women_wide, by = join_by(eppi_id)) |> 
#  relocate(final_human_decision, .after = last_col()) |> 
#  rowwise() |> 
#  mutate(
#    n_screeners = sum(!is.na(c_across(`Gyri Hval`:`Christine Hestevik`)))
#  ) |> 
#  ungroup()
#
#n_refs <- women_dat |> filter(n_screeners == 2) |> nrow()
#saveRDS(n_refs, "NIPH/Data/n references/women_n_refs.rds")
#
#women_dat_2screen <- 
#  women_dat |> 
#  # Removing train data plus single screened references
#  filter(n_screeners == 2) |> 
#  select(-n_screeners) |> 
#  pivot_longer(
#    cols = `Gyri Hval`:`Christine Hestevik`,
#    names_to = "screener",
#    values_to = "screener_decision"
#  ) |> 
#  filter(!is.na(screener_decision)) |> 
#  arrange(screener, final_human_decision) |>
#  relocate(screener:screener_decision, .before = final_human_decision)
#
#women_single_perform_dat_2screen <- 
#  women_dat_2screen |> 
#  summarise(
#    TP = sum(screener_decision == 1 & final_human_decision == 1, na.rm = TRUE),
#    TN = sum(screener_decision == 0 & final_human_decision == 0, na.rm = TRUE),
#    FN = sum(screener_decision == 0 & final_human_decision == 1, na.rm = TRUE),
#    FP = sum(screener_decision == 1 & final_human_decision == 0, na.rm = TRUE),
#    recall = TP / (TP + FN),
#    spec = TN / (TN + FP),
#    bacc = (recall + spec) / 2,
#    .by = screener
#  ) |> 
#  ungroup() |> 
#  mutate(
#    review_authors = "Vist et al., 2022",
#    review = "Women_health"
#  ) |> 
#  relocate(review_authors)
#
#saveRDS(women_single_perform_dat_2screen, "NIPH/Data/women_single_perform_dat_2screen.rds")
#
#rm(list = ls())

################################################################################
# 7. Loading data for (Ames et al., 2024)
################################################################################

html_WHO <- read_html("NIPH/WHO QES Chronic Pain/Screen on Title & Abstract full coding report_WHO QES Chronic Pain.html")

screen_report_raw_WHO <- 
  html_WHO |> 
  html_element("table") |> 
  html_table() |> 
  filter(str_detect(`I/E/D/S flag`, "(I)|(E)")) 

screen_report_raw_WHO |> glimpse()

screen_report_WHO <- 
  screen_report_raw_WHO |> 
  select(-`I/E/D/S flag`) |> 
  rename(
    eppi_id = ItemId,
    author_short = ShortTitle, 
    title_report = Title, 
    exclude1 = EXCLUDE,
    exclude2 = `Exclude MSK pain relevant`,
    include = `INCLUDE on title & abstract`
    
  ) |> 
  #mutate(
  #  across(exclude1:include2, ~ str_replace_all(.x, "Ã¥", "ae"))
  #) |> 
  arrange(eppi_id)

# Problem here
WHO_excl <- 
  revtools::read_bibliography("NIPH/WHO QES Chronic Pain/ExcludesT&A_WHOchronicpain_n801.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 0
  )

WHO_incl <- 
  revtools::read_bibliography("NIPH/WHO QES Chronic Pain/IncludesT&A_WHO_QES_Chronic_Pain_n187.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 1
  )

WHO_ris_dat <- bind_rows(WHO_incl, WHO_excl) 

#ids_WHO <- WHO_ris_dat$eppi_id
#
#screen_report_WHO_filtered <- 
#  screen_report_WHO |> 
#  filter(eppi_id %in% ids_WHO)

# Binding screen report and ris-file data together
WHO_dat_wide <- 
  left_join(screen_report_WHO, WHO_ris_dat, by = join_by(eppi_id)) |> 
  arrange(final_human_decision)

# Detecting individual screeners names
screeners_var_WHO <- 
  screen_report_WHO |> 
  reframe(
    name = unique(c_across(exclude1:include))
  ) |> 
  pull(name)


screeners_WHO <- 
  unlist(strsplit(screeners_var_WHO, "(?<=[a-z])(?=[A-Z])", perl = TRUE)) |> 
  gsub("\\[.*","", x = _) |> 
  unique() 

# Making long format data
filter_list <- list()

for (i in 1:length(screeners_WHO)){
  filter_list[[i]] <- 
    WHO_dat_wide |> 
    filter(if_any(exclude1:include, ~ str_detect(.x, screeners_WHO[i]))) |> 
    mutate(
      screener = screeners_WHO[i],
      across(exclude1:include, ~ str_extract(.x, screeners_WHO[i]))
    ) |> 
    relocate(screener)
}

single_screen_dat_WHO <- 
  filter_list |> 
  list_rbind() |> 
  mutate(
    exclude = if_any(exclude1:exclude2, ~ !is.na(.x)),
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

single_screen_dat_WHO_wide <- 
  single_screen_dat_WHO |> 
  pivot_wider(
    id_cols = eppi_id,
    id_expand = TRUE,
    values_from = screener_decision,
    names_from = screener,
  )

WHO_dat <- 
  left_join(WHO_dat_wide, single_screen_dat_WHO_wide, by = join_by(eppi_id)) |> 
  relocate(final_human_decision, .after = last_col()) |> 
  rowwise() |> 
  mutate(
    n_screeners = sum(!is.na(c_across(`Christine Hestevik`:`Melanie Ames`)))
  ) |> 
  ungroup()

n_refs <- WHO_dat |> filter(n_screeners == 2) |> nrow()
saveRDS(n_refs, "NIPH/Data/n references/WHO_n_refs.rds")

n_in <- WHO_dat |> filter(n_screeners == 2 & final_human_decision == 1) |> nrow()
saveRDS(n_in, "NIPH/Data/n references/WHO_n_in.rds")

WHO_dat_2screen <- 
  WHO_dat |> 
  # Removing train data plus single screened references
  filter(n_screeners == 2) |> 
  select(-n_screeners) |> 
  pivot_longer(
    cols = `Christine Hestevik`:`Melanie Ames`,
    names_to = "screener",
    values_to = "screener_decision"
  ) |> 
  filter(!is.na(screener_decision)) |> 
  arrange(screener, final_human_decision) |>
  relocate(screener:screener_decision, .before = final_human_decision)

WHO_single_perform_dat_2screen <- 
  WHO_dat_2screen |> 
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
    review_authors = "Ames et al. 2024",
    review = "WHO_QES_Chronic_Pain"
  ) |> 
  relocate(review_authors)

saveRDS(WHO_single_perform_dat_2screen, "NIPH/Data/WHO_single_perform_dat_2screen.rds")

rm(list = ls())

################################################################################
################################################################################
################################################################################
#
# Analyzing data
#
################################################################################
################################################################################
################################################################################

# Number of double-screened references
path_list <- list.files(path = "NIPH/Data/n references/", pattern = "_refs")

# Removing evidence and gap map
path_list <- path_list[-5]

n_references <- 
  map(path_list, ~ readRDS(paste0("NIPH/Data/n references/", .x))) |> 
  list_c() 

# Total number of references
n_references |> sum()

# Number of included double-screened references
path_list2 <- list.files(path = "NIPH/Data/n references/", pattern = "n_in")

# Removing evidence and gap map
path_list2 <- path_list2[-5]

n_in <- 
  map(path_list2, ~ readRDS(paste0("NIPH/Data/n references/", .x))) |> 
  list_c() 

tibble(
  review = str_remove_all(path_list, "_n_refs.rds"),
  n_references, 
  n_in
)

# Total number of references
n_references |> sum()

# Loading performance data
path <- list.files(path = "NIPH/Data/", pattern = "_dat")

# Removing evidence and gap map
path <- path[-5]

dat_raw <- 
  map(path, ~ readRDS(paste0("NIPH/Data/", .x))) |> 
  list_rbind() |> 
  filter() |> 
  rowwise() |> 
  mutate(
    nom = (TP*TN) - (FP*FN),
    denom1 = as.numeric(TP+FP),
    denom2 = as.numeric(TP+FN),
    denom3 = as.numeric(TN+FP),
    denom4 = as.numeric(TN+FN),
    denom = sqrt(denom1 * denom2 * denom3 * denom4),
    MCC = nom/denom,
    nMCC = (MCC+1)/2,
    N_recall = TP + FN,
    N_spec = TN + FP,
    N_bacc = N_recall  + N_spec,
    N_nMCC = TP + TN + FP + FN,
    n_recall = TP,
    n_spec = TN,
    n_bacc = TP + TN,
    n_nMCC = NA_real_
  ) |> 
  ungroup() |> 
  select(-c(nom:MCC)) |> 
  rename(
    val_recall = recall,
    val_spec = spec, 
    val_bacc = bacc,
    val_nMCC = nMCC
  ) |> 
  relocate(review, .after = review_authors) |> 
  # removing Jose  Meneses Echavez. Screening one relevant study only
  filter(screener != "Jose  Meneses Echavez")

# Numbers of screeners for paper
dat_raw |> 
  summarise(
    n_screeners = n(),
    .by = review_authors
  )

dat_long <- 
  dat_raw |> 
  pivot_longer(
    cols = -c(review_authors:FP),
    names_to = c('.value', 'Category'),
    names_sep = '_'
  ) |> 
  mutate(
    n = if_else(Category == 'bacc', round(val*N), n),
  ) |> 
  rename(metric = Category) |> 
  rowwise() |> 
  mutate(
    total_ref = sum(c_across(TP:FP))
  ) |> 
  #filter(total_ref > 500) |> 
  ungroup()

dat_long_prop <- 
  dat_long |> 
  filter(metric != "nMCC")

dat_long_cor <- 
  dat_long |> 
  filter(metric == "nMCC")

# Account for Schwarzer et al. critique
dat_trans_prop <- 
  metafor::escalc(measure="PAS", xi=n, ni=N, data=dat_long_prop) |> 
  mutate(
    esid = 1:n()
  ) 

dat_trans_cor <- 
  dat_long_cor |> 
  rowwise() |> 
  mutate(
    yi = 0.5 * log((1+val)/(1-val)),
    vi = 1/(N-3)
  ) |> 
  ungroup() |> 
  mutate(
    esid = 1:n()
  )

dat_trans <- 
  bind_rows(dat_trans_prop, dat_trans_cor) |> 
  mutate(
    metric = factor(metric, levels = c("recall", "spec", "bacc", "nMCC"))
  ) |> 
  arrange(review_authors, screener, metric) 

dat <- filter(dat_trans, metric == "recall")

V <- metafor::vcalc(vi, cluster = review_authors, obs = esid, rho = 0.7, data = dat)
V

V_mat <- clubSandwich::impute_covariance_matrix(dat$vi, cluster = dat$review_authors, r = 0.7)

CHE <- 
  metafor::rma.mv(
    yi ~ 0,
    V_mat,
    random = ~ 1 | review_authors / esid, 
    data = dat,
    sparse = TRUE
  ) |> 
  metafor::robust(
    cluster = review_authors, 
    clubSandwich = TRUE
  ) |> 
  suppressWarnings()

maCHEp <- function(filter, scale, data = dat_trans, rho = 0.7){
  
  dat <- 
    data |> 
    dplyr::filter(metric == filter)
  
  V <- metafor::vcalc(vi, cluster = review_authors, obs = esid, rho = rho, data = dat)
  V
  
  CHE <- 
    metafor::rma.mv(
      yi ~ 0,
      V,
      random = ~ 1 | review_authors / esid, 
      data = dat,
      sparse = TRUE
    ) |> 
    metafor::robust(
      cluster = review_authors, 
      clubSandwich = TRUE
    ) |> 
    suppressWarnings()
  
  if (scale == "prop"){
    
    model_res <- tibble(
      metric = filter,
      val = transf.iarcsin(CHE$beta),
      ci.lb = transf.iarcsin(CHE$ci.lb),
      ci.ub = transf.iarcsin(CHE$ci.ub),
      tau = sqrt(CHE$sigma2[1]),
      omega = sqrt(CHE$sigma2[2])
    ) 
    
  }
  
  if (scale == "cor"){
    
    b <- as.numeric(CHE$beta)  
    cil <- as.numeric(CHE$ci.lb) 
    ciu <- as.numeric(CHE$ci.ub)  
    
    model_res <- tibble(
      metric = filter,
      val = (exp(2*b)-1)/(exp(2*b) + 1),
      ci.lb = (exp(2*cil)-1)/(exp(2*cil) + 1),
      ci.ub = (exp(2*ciu)-1)/(exp(2*ciu) + 1),
      tau = sqrt(CHE$sigma2[1]),
      omega = sqrt(CHE$sigma2[2])
    ) 
    
  }
  
  res <- model_res
  res
  
}

params <- 
  tibble(
    filter = unique(dat_trans$metric),
    scale = rep(c("prop", "cor"), c(3, 1))
  )

all_res_list <- pmap(.l = params, .f = maCHEp) 

model_res_dat <- 
  all_res_list |> 
  list_rbind() |> 
  mutate(
    metric = case_when(
      metric == "recall" ~ "Recall",
      metric == "spec" ~ "Specificity",
      metric == "bacc" ~ "Balanced Accuracy",
      TRUE ~ metric
    ),
    metric = factor(metric, levels = c("Recall", "Specificity", "Balanced Accuracy", "nMCC")),
    role = "Researcher"
  )

# For paper
model_res_dat

# Transform back measures
dat_prop <- 
  summary(dat_trans_prop, transf=transf.iarcsin) |> 
  as_tibble() |> 
  mutate(
    metric = case_when(
      metric == "recall" ~ "Recall",
      metric == "spec" ~ "Specificity",
      metric == "bacc" ~ "Balanced Accuracy",
      TRUE ~ NA_character_
    ),
    metric = factor(metric, levels = c("Recall", "Specificity", "Balanced Accuracy"))
  ) |> 
  relocate(esid, .after = last_col())

dat_cor <- 
  dat_trans_cor |> 
  rowwise() |> 
  mutate(
    z_cil = yi - qnorm(0.975) * sqrt(vi),
    z_ciu = yi + qnorm(0.975) * sqrt(vi),
    yi = (exp(2*yi)-1)/(exp(2*yi) + 1),
    ci.lb = (exp(2*z_cil)-1)/(exp(2*z_cil) + 1),
    ci.ub =(exp(2*z_ciu)-1)/(exp(2*z_ciu) + 1)
  ) |> 
  ungroup() |> 
  select(!contains("z"), -vi) |> 
  relocate(esid, .after = last_col())

dat <- 
  bind_rows(dat_prop, dat_cor) |> 
  mutate(
    metric = factor(metric, levels = c("Recall", "Specificity", "Balanced Accuracy", "nMCC")),
    role = "Researcher"
  ) |> 
  arrange(review_authors, screener, metric)


vline_dat <- 
  model_res_dat  |> 
  select(role, metric, val)


# Recalculate order variable via metafor
png("NIPH/NIPH res figure.png", height = 5, width = 11, unit = "in", res = 600)
dat |> 
  mutate(
    order_var = weighted.mean(val, N),
    .by = c(review_authors, role, metric)
  ) |> 
  arrange(role, desc(order_var)) |> 
  mutate(
    review_authors = factor(review_authors, levels = unique(review_authors))
  ) |> 
  ggplot(aes(x = val, xmin = ci.lb, xmax = ci.ub, reorder_within(review_authors, desc(order_var), role), color = review_authors, alpha = 0.5)) + 
  geom_pointrange(position = position_dodge2(width = 0.5, padding = 0.5)) +
  geom_vline(data = vline_dat, aes(xintercept = val), linetype = 4) + 
  #scale_x_continuous(limits = c(0.4,1), breaks = seq(0L, 1L, 0.1)) +
  scale_y_reordered() + 
  facet_grid(role~metric, scales = "free") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.y = element_text(vjust = +3),
    axis.title.x = element_text(vjust = -0.75)
  ) +
  labs(x = "Estimate", y = "NIPH Systematic Review")
dev.off()