# Analyzing single reviewer data

library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(metafor)

options(pillar.sigfig = 4) # ensure tibble include 4 digits
#options(tibble.width = Inf)
#options(dplyr.print_min = 20)
options(scipen = 10)
options(dplyr.summarise.inform = FALSE) # Avoids summarize info from tidyverse

# Loading data

path <- list.files(path = "single screener data/", pattern = "_dat")

dat_raw <- 
  map(path, ~ readRDS(paste0("single screener data/", .x))) |> 
  list_rbind() |> 
  mutate(
    N_recall = TP + FN,
    N_spec = TN + FP,
    N_bacc = N_recall  + N_spec,
    prop_recall = TP,
    prop_spec = TN,
    prop_bacc = TP + TN
  ) |> 
  rename(
    perc_recall = recall,
    perc_spec = spec, 
    perc_bacc = bacc
  )

dat_long <- 
  dat_raw |> 
  pivot_longer(
    cols = -c(review_authors:FP),
    names_to = c('.value', 'Category'),
    names_sep = '_'
  ) |> 
  mutate(
    prop = if_else(Category == 'bacc', round(perc*N), prop)
  ) |> 
  rename(metric = Category)

# Account for Schwarzer et al. critique
dat_trans <- 
  escalc(measure="PAS", xi=prop, ni=N, data=dat_long) |> 
  mutate(
    esid = 1:n()
  )

# https://www.metafor-project.org/doku.php/analyses:miller1978?s[]=proportion
#res <- 
#  metafor::rma.mv(
#  yi,
#  vi,
#  random = ~ 1 | review_authors/esid,
#  data = filter(dat_trans, metric == "recall")
#) |> 
#  metafor::robust(
#    cluster = review_authors, 
#    clubSandwich = TRUE
#  )
#
#pred <- predict(res, transf=transf.iarcsin)
#pred

dat <- 
  summary(dat_trans, transf=transf.iarcsin) |> 
  as_tibble() |> 
  mutate(
    metric = factor(metric, levels = c("recall", "spec", "bacc")),
    role = factor(role, levels = c("Author", "Assistant"))
  ) 

vline_dat <- 
  dat |> 
  summarise(
    perc = mean(perc),
    .by = c(role, metric)
  ) 

dat |> 
#  mutate(
#    order_var = percent[1],
#    .by = review_authors
#  ) |> 
#  arrange(desc(order_var)) |> 
#  mutate(
#    review_authors = factor(review_authors, levels = unique(review_authors))
#  ) |> 
  ggplot(aes(x = perc, xmin = ci.lb, xmax = ci.ub, y = review_authors, color = review_authors, alpha = 0.5)) + 
  geom_pointrange(position = position_dodge2(width = 0.5, padding = 0.5)) +
  geom_vline(data = vline_dat, aes(xintercept = perc), linetype = "dashed") + 
  #scale_x_continuous(limits = c(0.4,1), breaks = seq(0L, 1L, 0.1)) +
  facet_grid(role~metric, scales = "free") +
  theme_bw() +
  theme(
    legend.position="none",
    axis.title.y = element_text(vjust = +3),
    axis.title.x = element_text(vjust = -0.75)
  ) +
  labs(x = "Percent (%)", y = "Campbell Systematic Review")

