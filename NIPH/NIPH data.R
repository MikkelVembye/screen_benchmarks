
library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)

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
women_excl <- 
  revtools::read_bibliography("NIPH/Womens health/ExcludesT&A_Womenshealth_n16028.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 0
  )

women_incl <- 
  revtools::read_bibliography("NIPH/Womens health/IncludesT&A_Womenshealth_n4605.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 1
  )

self_ris_dat <- bind_rows(self_incl, self_excl) 

ids_self <- self_ris_dat$eppi_id

screen_report_self_filtered <- 
  screen_report_self |> 
  filter(eppi_id %in% ids_self)

# Binding screen report and ris-file data together
self_dat_wide <- 
  left_join(screen_report_self_filtered, self_ris_dat, by = join_by(eppi_id)) |> 
  arrange(final_human_decision)

# Detecting individual screeners names
screeners_var_self <- 
  screen_report_self_filtered |> 
  reframe(
    name = unique(c_across(exclude:include))
  ) |> 
  pull(name)


screeners_self <- 
  unlist(strsplit(screeners_var_self, "(?<=[a-z])(?=[A-Z])", perl = TRUE)) |> 
  gsub("\\[.*","", x = _) |> 
  unique() 

# Making long format data
filter_list <- list()

for (i in 1:length(screeners_self)){
  filter_list[[i]] <- 
    self_dat_wide |> 
    filter(if_any(exclude:include, ~ str_detect(.x, screeners_self[i]))) |> 
    mutate(
      screener = screeners_self[i],
      across(exclude:include, ~ str_extract(.x, screeners_self[i]))
    ) |> 
    relocate(screener)
}

single_screen_dat_self <- 
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
    n_screeners = sum(!is.na(c_across(`Elisabet Hafstad`:`Kristin Thuve Dahm`)))
  ) |> 
  ungroup()

self_dat_2screen <- 
  self_dat |> 
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
    review_authors = "Meneses-Echavez et al. 2022)",
    review = "Psychological debriefing"
  ) |> 
  relocate(review_authors)

saveRDS(self_single_perform_dat_2screen, "NIPH/Data/self_single_perform_dat_2screen.rds")

rm(list = ls())

################################################################################
# 7. Loading data for (Ames et al., 2024)
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

self_ris_dat <- bind_rows(self_incl, self_excl) 

ids_self <- self_ris_dat$eppi_id

screen_report_self_filtered <- 
  screen_report_self |> 
  filter(eppi_id %in% ids_self)

# Binding screen report and ris-file data together
self_dat_wide <- 
  left_join(screen_report_self_filtered, self_ris_dat, by = join_by(eppi_id)) |> 
  arrange(final_human_decision)

# Detecting individual screeners names
screeners_var_self <- 
  screen_report_self_filtered |> 
  reframe(
    name = unique(c_across(exclude:include))
  ) |> 
  pull(name)


screeners_self <- 
  unlist(strsplit(screeners_var_self, "(?<=[a-z])(?=[A-Z])", perl = TRUE)) |> 
  gsub("\\[.*","", x = _) |> 
  unique() 

# Making long format data
filter_list <- list()

for (i in 1:length(screeners_self)){
  filter_list[[i]] <- 
    self_dat_wide |> 
    filter(if_any(exclude:include, ~ str_detect(.x, screeners_self[i]))) |> 
    mutate(
      screener = screeners_self[i],
      across(exclude:include, ~ str_extract(.x, screeners_self[i]))
    ) |> 
    relocate(screener)
}

single_screen_dat_self <- 
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
    n_screeners = sum(!is.na(c_across(`Elisabet Hafstad`:`Kristin Thuve Dahm`)))
  ) |> 
  ungroup()

self_dat_2screen <- 
  self_dat |> 
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
    review_authors = "Meneses-Echavez et al. 2022)",
    review = "Psychological debriefing"
  ) |> 
  relocate(review_authors)

saveRDS(self_single_perform_dat_2screen, "NIPH/Data/self_single_perform_dat_2screen.rds")

rm(list = ls())