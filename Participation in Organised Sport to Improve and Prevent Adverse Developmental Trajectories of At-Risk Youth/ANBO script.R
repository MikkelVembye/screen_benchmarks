# Creating data for sports review
library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)

library(readr)
Screen_on_Title_Abstract_full_coding_report_1_ <- read_csv("Screen on Title & Abstract full coding report (1).html")
View(Screen_on_Title_Abstract_full_coding_report_1_)


html <- read_html("Screen on Title & Abstract full coding report (1).html")

screen_report_dat_raw <- 
  html |> 
  html_element("table") |> 
  html_table()

# Til Anja: Du kan bruge glimpse() til at få overblik over variable navnene
screen_report_dat_raw |> glimpse()

# Read duplicate and delete refs 

miss_refs_ids <- readRDS("")

miss_refs_raw <- 
  screen_report_dat_raw |> 
  filter(ItemId %in% miss_refs_ids)
