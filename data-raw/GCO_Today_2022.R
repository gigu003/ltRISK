## code to prepare `GCO` dataset goes here
library(easyGCO)
library(dplyr)
data("GCO_Today")
data("WHO_Mortality")
data("WHO_Pop")
data("GCO_Sites")

pop <- WHO_Pop |>
  mutate(age = age - 1) |>
  filter(variant == 2) |>
  select(sex, age, popid, pop)

mort <- WHO_Mortality |>
  mutate(age = age - 1) |>
  filter(variant == 2) |>
  select(sex, age, popid, death)

#combine GCO incidence mortality and WHO population and all cause mortality.
GCO_Today_2022 <- GCO_Today |>
  left_join(mort, by = c("sex", "age", "popid")) |>
  left_join(pop, by = c("sex", "age", "popid")) |>
  filter(popid < 900) |>
  left_join(code_pop, by = c("popid")) |>
  select(-popid_gco) |>
  group_by(region, subregion, hdi_cat, popid, cancers, age) |>
  reframe(across(c("inci", "mort", "death", "pop"), sum)) |>
  group_by(cancers, age) |>
  reframe(across(c("inci", "mort", "death", "pop"), sum)) |>
  mutate(cancers = as.character(cancers)) |>
  left_join(GCO_Sites, by = c("cancers" = "site_code")) |>
  mutate(cancers = as.integer(cancers)) |>
  rename(
    site_code = cancers,
    cancer = inci,
    cancer_death = mort,
    pys = pop,
    ages = age
  ) |>
  relocate(icd10, site, site_abbr, .before = ages) |>
  mutate(ages = ages * 5) |>
  select(-site)

usethis::use_data(GCO_Today_2022, overwrite = TRUE)
