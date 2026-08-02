## code to prepare `DATASET` dataset goes here
library(readxl)
library(dplyr)
seer2019_2022 <- read_excel("data-raw/data.xlsx", sheet = "seer2019_2022")
usethis::use_data(seer2019_2022, overwrite = TRUE)
