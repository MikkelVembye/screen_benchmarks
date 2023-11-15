# Calculating recall and specificity for co-teaching review
library(readr)
library(dplyr)

screen_measures <- readr::read_csv("co-teaching/screening_measures.csv")

TP <- screen_measures$`A Yes, B Yes`
TN <- screen_measures$`A No, B No`

n_conflicts <- screen_measures$`A Yes, B No` + screen_measures$`A No, B Yes`

FN <- 373 - TP
FP <- n_conflicts - FN

# From PRISMA Figure 1 (Vembye et al. 2023)
Recall <- TP/(TP + FN)
Specificity <- TN/(TN + FP)

bAcc <- (Recall + Specificity)/2

coteach_dat <- 
  tibble(
   review_authors = "Vembye et al. (2023)",
   review = "Co-teaching",
   number_of_references_0 = 7604, # The screening report from Covidence lacked 30 studies which are excluded here
   number_of_references_1 = 373,
   number_of_conflicts_0 = FP, 
   number_of_conflicts_1 = FN,
   metric = factor(c("Recall", "Specificity", "bAcc"), levels = c("Recall", "Specificity", "bAcc")),
   percent = c(Recall, Specificity, bAcc)
  )

saveRDS(coteach_dat, "all data sets/coteach_measure.rds")
