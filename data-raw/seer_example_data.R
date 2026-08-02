## code to prepare `breast` dataset goes here
library(readxl)
library(dplyr)
seer_example_data <- read_excel("data-raw/data.xlsx", sheet = "seer")
usethis::use_data(seer_example_data, overwrite = TRUE)
