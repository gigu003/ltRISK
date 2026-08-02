## code to prepare `DATASET` dataset goes here
library(readxl)
library(dplyr)
library(tidyr)
inci <- read_excel("data-raw/Beijing2021.xlsx", sheet = "inci")
mort <- read_excel("data-raw/Beijing2021.xlsx", sheet = "mort")
pop <- read_excel("data-raw/Beijing2021.xlsx", sheet = "pop")

pop <- pop |>
  group_by(year, sex) |>
  mutate(across(
    c(pys, death),
    ~ replace(.x, 1, .x[1] + .x[2])
  )) |>
  slice(-2)
data <- inci |>
  bind_rows(mort) |>
  mutate(a0 = a0 + a1) |>
  select(-a1) |>
  pivot_longer(cols = starts_with("a"), names_to = "age", values_to = "cc") |>
  pivot_wider(names_from = "type", values_from = "cc") |>
  mutate(age = as.numeric(gsub("[^0-9.]", "", age))) |>
  filter(!icdgroup %in% c("其他", "O&U")) |>
  left_join(pop, by = c("year", "sex", "age"))


total <- data |>
  filter(icdgroup == "C33~C34", sex == 2) |>
  group_by(year, sex, icdgroup)


ltr <- data |>
  group_by(year, sex, icdgroup) |>
  reframe(
    lt = round(
      ltr_devcan(
        ages = age,
        cancer = cancer,
        death = death,
        cancer_death = cancer_death1,
        pys = pys
      )$estimate$estimate *
        100,
      2
    ),
    lt1 = round(
      ltr_devcan(
        ages = age,
        cancer = cancer1,
        death = death,
        cancer_death = cancer_death1,
        pys = pys
      )$estimate$estimate *
        100,
      2
    ),
    lt2 = estimate(ltr_amp(
      cancer = cancer,
      death = death,
      cancer_death = cancer_death1,
      pys = pys
    ))$risk,
    rate1 = round(sum(cancer) / sum(pys) * 100000, 2),
    rate2 = round(sum(cancer1) / sum(pys) * 100000, 2),
    multi = sum(cancer) - sum(cancer1),
    r_multi = round(multi / sum(cancer) * 100, 2)
  ) |>
  mutate(diff = round((lt2 - lt1) / lt1 * 100, 2))
library(openxlsx)
write.xlsx(ltr, "result.xlsx")
