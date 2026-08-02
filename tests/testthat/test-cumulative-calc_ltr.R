test_that("calc_ltr supports cumulative risk function", {
  data("seer_example_data")
  all_sites <- seer_example_data[
    seer_example_data$site == "All" & seer_example_data$sex == 0,
  ]

  res <- calc_ltr(
    ages = all_sites$ages,
    cancer = all_sites$cancer,
    cancer_death = all_sites$cancer_death,
    death = all_sites$death,
    pys = all_sites$pys,
    risk_func = "cumulative",
    type = "developing",
    maj_method = "constant",
    ci_method = "delta",
    age_start = 0,
    age_end = c(75, 85),
    digits = 6
  )

  obj <- cumulative(
    ages = all_sites$ages,
    cancer = all_sites$cancer,
    cancer_death = all_sites$cancer_death,
    death = all_sites$death,
    pys = all_sites$pys,
    type = "developing",
    maj_method = "constant"
  )

  expect_equal(res$risk, round(get_risk(obj, 0, c(75, 85)) * 100, 6))
})

test_that("legacy risk_func cumu matches cumulative", {
  data("seer_example_data")
  all_sites <- seer_example_data[
    seer_example_data$site == "All" & seer_example_data$sex == 0,
  ]

  args <- list(
    ages = all_sites$ages,
    cancer = all_sites$cancer,
    cancer_death = all_sites$cancer_death,
    death = all_sites$death,
    pys = all_sites$pys,
    type = "developing",
    maj_method = "constant",
    ci_method = "delta",
    age_start = 0,
    age_end = 75,
    digits = 6
  )

  res_cumulative <- do.call(calc_ltr, c(args, list(risk_func = "cumulative")))
  res_cumu <- do.call(calc_ltr, c(args, list(risk_func = "cumu")))

  expect_equal(res_cumu, res_cumulative)
})

test_that("cumulative reports valid type values", {
  data("seer_example_data")
  all_sites <- seer_example_data[
    seer_example_data$site == "All" & seer_example_data$sex == 0,
  ]

  expect_error(
    cumulative(
      ages = all_sites$ages,
      cancer = all_sites$cancer,
      cancer_death = all_sites$cancer_death,
      death = all_sites$death,
      pys = all_sites$pys,
      type = "death"
    ),
    "type must be either 'developing' or 'dying'"
  )
})
