test_that("calc_ltr keeps default output unchanged when variance is not requested", {
  data("seer_example_data")
  breast <- seer_example_data[seer_example_data$site == "Breast", ]

  res <- calc_ltr(
    ages = breast$ages,
    cancer = breast$cancer,
    cancer_death = breast$cancer_death,
    death = breast$death,
    pys = breast$pys,
    maj_method = "constant",
    ci_method = "delta",
    age_start = 0,
    age_end = Inf,
    digits = 6
  )

  expect_named(res, c("start", "end", "risk", "lower", "upper"))
})

test_that("calc_ltr can return variance and standard error", {
  data("seer_example_data")
  breast <- seer_example_data[seer_example_data$site == "Breast", ]

  res <- calc_ltr(
    ages = breast$ages,
    cancer = breast$cancer,
    cancer_death = breast$cancer_death,
    death = breast$death,
    pys = breast$pys,
    maj_method = "constant",
    ci_method = "delta",
    age_start = c(0, 40),
    age_end = Inf,
    digits = 6,
    return_variance = TRUE
  )

  expect_named(
    res,
    c("start", "end", "risk", "lower", "upper", "variance", "se")
  )
  expect_true(all(res$variance > 0))
  expect_equal(res$se^2, res$variance)
})

test_that("calc_ltr variance uses the requested multiplier scale", {
  data("seer_example_data")
  breast <- seer_example_data[seer_example_data$site == "Breast", ]

  res_unit <- calc_ltr(
    ages = breast$ages,
    cancer = breast$cancer,
    cancer_death = breast$cancer_death,
    death = breast$death,
    pys = breast$pys,
    maj_method = "constant",
    ci_method = "delta",
    age_start = 0,
    age_end = Inf,
    multiplier = 1,
    digits = 10,
    return_variance = TRUE
  )

  res_percent <- calc_ltr(
    ages = breast$ages,
    cancer = breast$cancer,
    cancer_death = breast$cancer_death,
    death = breast$death,
    pys = breast$pys,
    maj_method = "constant",
    ci_method = "delta",
    age_start = 0,
    age_end = Inf,
    multiplier = 100,
    digits = 10,
    return_variance = TRUE
  )

  expect_equal(res_percent$se, res_unit$se * 100)
  expect_equal(res_percent$variance, res_unit$variance * 100^2)
})
