# Analyzing single reviewer data

library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(metafor)
library(tidytext)
library(stringr)


options(pillar.sigfig = 4) # ensure tibble include 4 digits
#options(tibble.width = Inf)
#options(dplyr.print_min = 20)
options(scipen = 10)
options(dplyr.summarise.inform = FALSE) # Avoids summarize info from tidyverse

# Number of double-screened references
path_list <- list.files(path = "single screener data/Number of References/", pattern = "n_refs")

n_references <- 
  map(path_list, ~ readRDS(paste0("single screener data/Number of References/", .x))) |> 
  list_c()

path_list2 <- list.files(path = "single screener data/Number of References/", pattern = "n_in")

n_in <- 
  map(path_list2, ~ readRDS(paste0("single screener data/Number of References/", .x))) |> 
  list_c()

review_name <- path_list |> str_remove_all("_n_refs.rds")

# For table in paper
descrip_info <- tibble(review_name, n_in, n_references)
descrip_info

#Total number of references
N_refs <- n_references |> sum()
N_refs


# Loading performance data
path <- list.files(path = "single screener data/", pattern = "dat_2")

dat_raw <- 
  map(path, ~ readRDS(paste0("single screener data/", .x))) |> 
  list_rbind() |> 
  filter(screener != "Erika Lundqvist") |> 
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
  mutate(
    screener = case_when(
      str_detect(screener, "ildemoes") ~ "Malene Wallach Kildemoes",
      TRUE ~ screener
    )
  )

# Number of screeners across studies

dat_raw |> 
  summarise(
    n_ass = sum(role == "Assistant"),
    n_aut = sum(role == "Author"),
    .by = review
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
  arrange(review_authors, role, screener, metric) 

# Build analysis function

# https://www.metafor-project.org/doku.php/analyses:miller1978?s[]=proportion

maSCEp <- function(filter, scale, data = dat_trans, rho = 0.7){
  
  dat <- 
    data |> 
    dplyr::filter(metric == filter)
  
  V <- metafor::vcalc(vi, cluster = review_authors, subgroup = role, obs = esid, rho = rho, data = dat)
  V
  
  sce <- 
    metafor::rma.mv(
      yi ~ 0 + role,
      V,
      random = list(~ role | review_authors, ~ role | esid), 
      struct = c("DIAG", "DIAG"),
      data = dat,
      sparse = TRUE
    ) |> 
    metafor::robust(
      cluster = review_authors, 
      clubSandwich = TRUE
    )
  
  wald_test <- 
    clubSandwich::Wald_test(
      sce, 
      constraints = clubSandwich::constrain_equal(1:2), 
      vcov = "CR2"
    ) |> 
    dplyr::mutate(
      metric = filter,
      p_val = as.numeric(p_val)
    ) |> 
    dplyr::relocate(metric)
  
  
  if (scale == "prop"){
    
   model_res <- tibble(
     metric = filter,
     role = str_remove_all(rownames(sce$b), "role"),
     val = transf.iarcsin(sce$beta),
     ci.lb = transf.iarcsin(sce$ci.lb),
     ci.ub = transf.iarcsin(sce$ci.ub),
     tau = sqrt(transf.iarcsin(sce$tau2)),
     omega = sqrt(transf.iarcsin(sce$gamma2))
   ) 
    
  }
  
  if (scale == "cor"){
  
  b <- as.numeric(sce$beta)  
  cil <- as.numeric(sce$ci.lb) 
  ciu <- as.numeric(sce$ci.ub)  
  
  model_res <- tibble(
    metric = filter,
    role = str_remove_all(rownames(sce$b), "role"),
    val = (exp(2*b)-1)/(exp(2*b) + 1),
    ci.lb = (exp(2*cil)-1)/(exp(2*cil) + 1),
    ci.ub = (exp(2*ciu)-1)/(exp(2*ciu) + 1),
    tau = sqrt((exp(2*sce$tau2)-1)/(exp(2*sce$tau2) + 1)),
    omega = sqrt((exp(2*sce$gamma2)-1)/(exp(2*sce$gamma2) + 1))
  ) 
    
  }
  
  res <- list(model_res = model_res, wald_test = wald_test)
  res
  
}

params <- 
  tibble(
    filter = unique(dat_trans$metric),
    scale = rep(c("prop", "cor"), c(3, 1))
  )

all_res_list <- pmap(.l = params, .f = maSCEp) 

model_res_dat <- 
  map(1:4, ~ all_res_list[[.x]][[1]]) |> 
  list_rbind() |> 
  mutate(
    metric = case_when(
      metric == "recall" ~ "Recall",
      metric == "spec" ~ "Specificity",
      metric == "bacc" ~ "Balanced Accuracy",
      TRUE ~ metric
    ),
    metric = factor(metric, levels = c("Recall", "Specificity", "Balanced Accuracy", "nMCC")),
    role = if_else(role == "Assistant", "Assistant / Non-Content Expert", role),
    role = factor(role, levels = c("Assistant / Non-Content Expert", "Author"))
  ) |> 
  arrange(metric)
  

# For paper only 
wald_res_dat <- 
  map(1:4, ~ all_res_list[[.x]][[2]]) |> 
  list_rbind()


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
    role = if_else(role == "Assistant", "Assistant / Non-Content Expert", role),
    role = factor(role, levels = c("Assistant / Non-Content Expert", "Author"))
  ) |> 
  arrange(review_authors, screener, metric)

# Prepare polygons
#r_diam_x1 <- r_diam_y1 <- 
#  dat |> 
#  filter(role == "Assistant / Non-Content Expert" & metric == "Recall") |> 
#  nrow() - 2
#
#r_diam_x2 <- r_diam_y2 <- 
#  dat |> 
#  filter(role == "Author" & metric == "Recall") |> 
#  nrow() - 2
#
#sum.x1 <- map(1:4, ~ 
#  c(
#    rep(NA, r_diam_x1),
#    model_res_dat$ci.lb[.x], 
#    model_res_dat$val[.x], 
#    model_res_dat$ci.ub[.x], 
#    model_res_dat$val[.x]
#    
#  )
#  ) |>
#  list_c()
#  
#sum.x2 <- map(5:8, ~ 
#  c(
#    rep(NA, r_diam_x2),
#    model_res_dat$ci.lb[.x], 
#    model_res_dat$val[.x], 
#    model_res_dat$ci.ub[.x], 
#    model_res_dat$val[.x]
#    
#    )
#  ) |>
#  list_c()
#
#sum.y1 <- rep(c(rep(NA, r_diam_y1), 1, 0.7, 1, 1.3), 4)
#sum.y2 <- rep(c(rep(NA, r_diam_y2), 1, 0.7, 1, 1.3), 4)


vline_dat <- 
  model_res_dat  |> 
  select(role, metric, val) |> 
  arrange(role) |> 
  filter(metric != "nMCC") 

# Recalculate order variable via metafor
#png("single screener data/Figures/facet_grid fig.png", height = 7, width = 12, unit = "in", res = 600)
dat_prop |> 
mutate(
  role = if_else(role == "Assistant", "Assistant / Non-Content Expert", role),
  role = factor(role, levels = c("Assistant / Non-Content Expert", "Author")),
  review_authors = if_else(review_authors == "Filges et al. (forthcoming)", "Filges, Smedslund et al. (2023)", review_authors)
) |> 
mutate(
  order_var = weighted.mean(val, N),
  .by = c(review_authors, role, metric)
) |> 
#group_by(role, metric) |> 
#group_modify(~ add_row(.x, order_var = 1)) |> 
#group_modify(~ add_row(.x, review_authors = "Summary (SCE+)")) |> 
#mutate(
#  
#  review_authors = replace_na(review_authors, ""),
#  order_var = if_else(review_authors == "Summary (SCE+)", 1.1, order_var),
#  
#) |> 
#ungroup() |>  
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
#geom_polygon(aes(x=c(sum.x1, sum.x2), y=c(sum.y1, sum.y2)), color = "black", alpha = 1) +
theme_bw() +
theme(
  legend.position="none",
  axis.title.y = element_text(vjust = +3),
  axis.title.x = element_text(vjust = -0.75)
) +
labs(x = "Estimate", y = "Campbell Systematic Review")
#dev.off()

