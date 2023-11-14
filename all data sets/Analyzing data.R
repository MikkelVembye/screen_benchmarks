# Analyzing conflict rates in VIVE Campbell Systematic Reviews
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(forcats)

# Options
options(pillar.sigfig = 4) # ensure tibble include 4 digits
options(tibble.width = Inf)
options(dplyr.print_min = 20)
options(scipen = 10)
options(dplyr.summarise.inform = FALSE) # Avoids summarize info from tidyverse

# Loading data
path <- list.files("all data sets/", pattern = "rds")

dat <- 
  map(path, ~ readRDS(paste0("all data sets/", .x))) |> 
  list_rbind()

# Overall conflict rates
overall_rate <- 
  dat |> 
  summarise(
    number_of_references = n(),
    number_of_conflicts = sum(conflict == 1), 
    percent_conflict = mean(conflict == 1),
    percent = 1 - percent_conflict,
    .by = final_human_decision
  ) |> 
  mutate(
    metric = c("Specificity", "Recall")
  )

bacc_overall <- sum(overall_rate$percent)/2
bacc_overall

# Conflict rates across review
dat |> 
  summarise(
    number_of_references = n(),
    number_of_conflicts = sum(conflict == 1), 
    percent_conflict = mean(conflict == 1),
    .by = c(review, final_human_decision)
  )


# Visualizing conflict rates across review
plot_dat <- 
  dat |> 
  summarise(
    number_of_references = n(),
    number_of_conflicts = sum(conflict == 1), 
    percent_conflict = mean(conflict == 1),
    .by = c(review_authors, review, final_human_decision)
  ) |> 
  pivot_wider(
    names_from = final_human_decision,
    values_from = number_of_references:percent_conflict
  ) |> 
  rename(Specificity = percent_conflict_0, Recall = percent_conflict_1) |> 
  mutate(
    Specificity = 1 - Specificity,
    Recall = 1-Recall,
    bAcc = (Specificity + Recall)/2
  ) |> 
  pivot_longer(
    cols = Specificity:bAcc,
    values_to = "percent",
    names_to = "metric"
  ) |> 
  mutate(
    metric = factor(metric, levels = c("Recall", "Specificity", "bAcc"))
  )

vline_dat <- 
  plot_dat |> 
  summarise(
    percent = mean(percent),
    .by = metric
  )


plot_dat |> 
ggplot(aes(x = percent, y = review_authors, color = review_authors)) + 
  geom_point() +
  geom_vline(data = vline_dat, aes(xintercept = percent), linetype = "dashed") + 
  scale_x_continuous(limits = c(0.5,1), breaks = seq(0L, 1L, 0.1)) +
  facet_grid(~metric) +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.y = element_text(vjust = +3),
    axis.title.x = element_text(vjust = -0.75)
  ) +
  labs(x = "%", y = "Campbell systematic review")
