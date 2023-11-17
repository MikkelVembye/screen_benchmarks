# Creating data for foster adoptive review
library(tidyverse)
library(rvest)
library(revtools)
library(purrr)
library(openxlsx2)
library(stringr)
library(readr)

Screen_on_title_abstract <- read_csv("Screen on Title & Abstract full coding report.html")
View(Screen_on_title_abstract)


html <- read_html("Screen on Title & Abstract full coding report.html")

screen_report_dat_raw <- 
  html |> 
  html_element("table") |> 
  html_table()

# Til Anja: Du kan bruge glimpse() til at få overblik over variable navnene
screen_report_dat_raw |> glimpse()

# ANBO: Hertil har det fungeret fint og er kørt som det skal. Jeg har uploadet RIS-filer samt coding report. 

