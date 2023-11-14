library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)

html <- read_html("Asylum/Asylum screening level 1 full coding report.html")

screen_report_dat_raw <- 
  html |> 
  html_element("table") |> 
  html_table()
