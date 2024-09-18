# Creating data for class size in special education
library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)
library(readr)

html <- read_html("Class size special edu/Class size spec edu screen on title & abstract full coding report.html")

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
    exclude = EXCLUDE,
    include = `INCLUDE on title & abstract`
  ) |> 
#  mutate(
#    across(exclude:include, ~ str_replace_all(.x, "Malene  Wallach Kildemoes", "Screener1")),
#    across(exclude:include, ~ str_replace_all(.x, "Juliane  Esper Ramstedt", "Screener2")),
#    across(exclude:include, ~ str_replace_all(.x, "Anja Bondebjerg", "Screener3")),
#    across(exclude:include, ~ str_replace_all(.x, "Maluhs Christensen", "Screener4")),
#    across(exclude:include, ~ str_replace_all(.x, "Nina Thorup Dalgaard", "Screener5")),
#    across(exclude:include, ~ str_replace_all(.x, "Katrine Nielsen", "Screener6"))
#  ) |> 
  arrange(eppi_id) 


#rm(screen_report_dat_raw)

# Loading included and excluded studies
ex_paths <- list.files("Class size special edu/", pattern = "Excl")

spec_edu_excl <- 
  map(ex_paths, ~ {
    revtools::read_bibliography(paste0("Class size special edu/", .x)) |> 
      suppressWarnings() |> 
      as_tibble() |>
      select(author, eppi_id, title, abstract) |> # Using only relevant variables
      mutate(
        final_human_decision = 0
      )
  }
  ) |> 
  list_rbind()

spec_edu_incl <- 
  revtools::read_bibliography("Class size special edu/Includetitleabstract.ris") |> 
  suppressWarnings() |> 
  as_tibble() |>
  select(author, eppi_id, title, abstract) |> # Using only relevant variables
  mutate(
    final_human_decision = 1
  )

spec_edu_ris_dat <- bind_rows(spec_edu_excl, spec_edu_incl) 

spec_edu_dat_wide <- 
  left_join(screen_report_dat, spec_edu_ris_dat, by = join_by(eppi_id)) |> 
  arrange(final_human_decision)

#rm(spec_edu_excl, spec_edu_incl)

#screeners <- paste0("Screener", 1:6)

# Screener 1 = Assistant
# Screener 2 = Assistant
# Screener 3 = Author
# Screener 4 = Assistant
# Screener 5 = Author
# Screener 6 = Assistant
 

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
    spec_edu_dat_wide |> 
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

#rm(filter_list)

#Check 
sum(single_screen_dat$screener_decision == 2)

spec_edu_single_perform_dat <- 
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
    review_authors = "Bondebjerg et al. (2023)",
    review = "Class size (special edu)",
    role = c("Assistant", "Assistant", "Author", "Assistant", "Author", "Assistant")
  ) |> 
  relocate(review_authors:role)

saveRDS(spec_edu_single_perform_dat, "single screener data/All screenings/spec_edu_single_perform_dat.rds")

# Extracting all individual screener scores in wide format to exclude training references
single_screen_dat_wide <- 
  single_screen_dat |> 
  pivot_wider(
    id_cols = eppi_id,
    id_expand = TRUE,
    values_from = screener_decision,
    names_from = screener,
  )

spec_edu_dat <- 
  left_join(spec_edu_dat_wide, single_screen_dat_wide, by = join_by(eppi_id)) |> 
  relocate(final_human_decision, .after = last_col()) |> 
  rowwise() |> 
  mutate(
    n_screeners = sum(!is.na(c_across(`Malene  Wallach Kildemoes`:`Katrine Nielsen`)))
  ) |> 
  ungroup()

n_refs <- spec_edu_dat |> filter(n_screeners == 2) |> nrow()
saveRDS(n_refs, "single screener data/Number of References/spec_edu_n_refs.rds")

n_in <- spec_edu_dat |> filter(n_screeners == 2 & final_human_decision == 1) |> nrow()
saveRDS(n_in, "single screener data/Number of References/spec_edu_n_in.rds")

cor_dat <- 
  spec_edu_dat |> 
  filter(n_screeners == 2) |> 
  select(`Malene  Wallach Kildemoes`:`Katrine Nielsen`) |> 
  mutate(
    across(everything(), ~ as.integer(.x))
  ) |> 
  relocate(`Anja Bondebjerg`) |> 
  relocate(`Juliane  Esper Ramstedt`, .after = `Anja Bondebjerg`) |> 
  relocate(`Katrine Nielsen`, .after = `Juliane  Esper Ramstedt`) |> 
  relocate(`Malene  Wallach Kildemoes`, .after = `Katrine Nielsen`) |> 
  relocate(`Maluhs Christensen`, .after = `Malene  Wallach Kildemoes`) |> 
  relocate(`Nina Thorup Dalgaard`, .after = last_col())

#constant_cor <- 0.5
#cor_mat <- 
#  cor(cor_dat, use = "pairwise.complete.obs") |> 
#  as_tibble() |> 
#  mutate(
#    Screener7 = NA_real_,
#    across(Screener1:Screener7, ~ if_else(!all(is.na(.x)) & is.na(.x), constant_cor, .x))
#  )

cor_mat <- 
  cor(cor_dat, use = "pairwise.complete.obs") |> 
  as.data.frame() |> 
  remove_rownames() |> 
  mutate(
    across(everything(), ~ if_else(is.na(.x), 0, .x))
  )
  
  
colnames(cor_mat) <- paste0("r", 1:ncol(cor_mat))

  
#names(cor_mat) <- paste0("r", 1:6)
#saveRDS(cor_mat, "single screener data/Between screener correlation/spec_edu_cor_mat.rds")  

spec_edu_dat_2screen <- 
  spec_edu_dat |> 
  # Removing train data plus uncertainty decisions
  filter(n_screeners == 2) |> 
  select(-n_screeners) |> 
  pivot_longer(
    cols = `Malene  Wallach Kildemoes`:`Katrine Nielsen`,
    names_to = "screener",
    values_to = "screener_decision"
  ) |> 
  filter(!is.na(screener_decision)) |> 
  arrange(screener, final_human_decision) |>
  relocate(screener:screener_decision, .before = final_human_decision) |> 
  mutate(review = "Class size (special edu)") |> 
  relocate(review)

saveRDS(spec_edu_dat_2screen, "single screener data/Raw data/spec_edu_dat_2screen.rds")

spec_edu_single_perform_dat_2screen <- 
  spec_edu_dat_2screen |> 
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
    
    review_authors = "Bondebjerg et al. (2023)",
    review = "Class size (special edu)",
    role = rep(c("Author", "Assistant", "Author"), c(1, 4, 1))
  ) |> 
  relocate(review_authors:role) |> 
  bind_cols(cor_mat)

#left_join(spec_edu_single_perform_dat_2screen, cor_mat, by = join_by(screener))

saveRDS(spec_edu_single_perform_dat_2screen, "single screener data/spec_edu_single_perform_dat_2screen.rds")

# Creating data with with correlation estimates --------------------------------

dat_long <- 
  spec_edu_single_perform_dat_2screen |> 
  mutate(
    N_recall = TP + FN,
    N_spec = TN + FP,
    N_bacc = N_recall + N_spec,
    n_recall = TP,
    n_spec = TN,
    n_bacc = TP + TN
  ) |> 
  rename(
    val_recall = recall,
    val_spec = spec, 
    val_bacc = bacc
  ) |> 
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
  ungroup()

dat_trans_prop <- 
  metafor::escalc(measure="PAS", xi=n, ni=N, data=dat_long) |> 
  mutate(
    esid = 1:n()
  )


save_varcov <- function(measure){
  
  dat <- 
    dat_trans_prop |> 
    filter(str_detect(metric, measure)) |> 
    bind_cols(cor_mat)
  
  v_mat <- 
    metafor::vcalc(vi, cluster = review_authors, rvars = c(r1:r6), data = dat) |> 
    suppressWarnings()
  
  saveRDS(
    v_mat, 
    file = paste0("single screener data/varcov/", measure,"/spec_edu_v_mat_", measure, ".rds")
    )
  
}

map(unique(dat_trans_prop$metric), .f = save_varcov)

  


