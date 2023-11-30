# Analyzing single reviewer data

library(dplyr)
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

# Loading data

path <- list.files(path = "single screener data/", pattern = "_dat")

dat_raw <- 
  map(path, ~ readRDS(paste0("single screener data/", .x))) |> 
  list_rbind() |> 
  #filter(screener != "Erika Lundqvist") |> 
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
  )

dat_long <- 
  dat_raw |> 
  pivot_longer(
    cols = -c(review_authors:FP),
    names_to = c('.value', 'Category'),
    names_sep = '_'
  ) |> 
  mutate(
    n = if_else(Category == 'bacc', round(val*N), n)
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
  escalc(measure="PAS", xi=n, ni=N, data=dat_long_prop) |> 
  mutate(
    esid = 1:n()
  )

dat_trans_cor <- 
  dat_long_cor |> 
  rowwise() |> 
  mutate(
    z = 0.5 * log((1+val)/(1-val)),
    z_vi = 1/(N-3)
  ) |> 
  ungroup() |> 
  mutate(
    esid = 1:n()
  )


# Build analysis function

# https://www.metafor-project.org/doku.php/analyses:miller1978?s[]=proportion
res <- 
  metafor::rma.mv(
  yi,
  vi,
  random = ~ 1 | review_authors/esid,
  data = filter(dat_trans_prop, metric == "recall" & role == "Assistant")
) |> 
  metafor::robust(
    cluster = review_authors, 
    clubSandwich = TRUE
  )

pred <- predict(res, transf=transf.iarcsin)
pred

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
    metric = factor(metric, levels = c("Recall", "Specificity", "Balanced Accuracy")),
    role = factor(role, levels = c("Author", "Assistant"))
  ) |> 
  relocate(esid, .after = last_col())

dat_cor <- 
  dat_trans_cor |> 
  rowwise() |> 
  mutate(
    z_cil = z - qnorm(0.975) * sqrt(z_vi),
    z_ciu = z + qnorm(0.975) * sqrt(z_vi),
    yi = (exp(2*z)-1)/(exp(2*z) + 1),
    ci.lb = (exp(2*z_cil)-1)/(exp(2*z_cil) + 1),
    ci.ub =(exp(2*z_ciu)-1)/(exp(2*z_ciu) + 1)
  ) |> 
  ungroup() |> 
  select(!contains("z")) |> 
  relocate(esid, .after = last_col())

dat <- 
  bind_rows(dat_prop, dat_cor) |> 
  mutate(
    metric = factor(metric, levels = c("Recall", "Specificity", "Balanced Accuracy", "nMCC")),
    role = if_else(role == "Assistant", "Assistant / Non-Content Expert", role),
    role = factor(role, levels = c("Assistant / Non-Content Expert", "Author"))
  ) |> 
  arrange(review_authors, screener, metric)

vline_dat <- 
  dat |> 
  summarise(
    val = weighted.mean(val, N),
    .by = c(role, metric)
  ) 

# Recalculate order variable via metafor
png("single screener data/Figures/facet_grid fig.png", height = 7, width = 12, unit = "in", res = 600)
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
geom_vline(data = vline_dat, aes(xintercept = val), linetype = "dashed") + 
#scale_x_continuous(limits = c(0.4,1), breaks = seq(0L, 1L, 0.1)) +
scale_y_reordered() +
facet_grid(role~metric, scales = "free") +
theme_bw() +
theme(
  legend.position="none",
  axis.title.y = element_text(vjust = +3),
  axis.title.x = element_text(vjust = -0.75)
) +
labs(x = "Estimate", y = "Campbell Systematic Review")
dev.off()
