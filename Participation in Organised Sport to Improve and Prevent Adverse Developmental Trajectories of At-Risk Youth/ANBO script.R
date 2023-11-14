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