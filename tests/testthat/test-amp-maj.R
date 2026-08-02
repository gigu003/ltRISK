test_that("amp supports maj_method arguments", {
  data("seer_example_data")
  all_sites <- seer_example_data[
    seer_example_data$site == "All" & seer_example_data$sex == 0,
  ]

  res_constant <- amp(
    ages = all_sites$ages,
    cancer = all_sites$cancer,
    cancer_death = all_sites$cancer_death,
    death = all_sites$death,
    pys = all_sites$pys,
    maj_method = "constant"
  )

  res_pmaj <- amp(
    ages = all_sites$ages,
    cancer = all_sites$cancer,
    cancer_death = all_sites$cancer_death,
    death = all_sites$death,
    pys = all_sites$pys,
    maj_method = "pmaj",
    pmaj_sub_interval = 1
  )

  expect_s3_class(res_constant, "amp")
  expect_s3_class(res_pmaj, "amp")
  expect_named(
    res_constant,
    c("risk", "ages", "contrib_a", "S0ai", "fracs", "rem_rates", "widths")
  )
  expect_true(length(res_pmaj$ages) > length(res_constant$ages))
})

test_that("amp constant method preserves previous piecewise-constant calculation", {
  data("seer_example_data")
  all_sites <- seer_example_data[
    seer_example_data$site == "All" & seer_example_data$sex == 0,
  ]

  res <- amp(
    ages = all_sites$ages,
    cancer = all_sites$cancer,
    cancer_death = all_sites$cancer_death,
    death = all_sites$death,
    pys = all_sites$pys,
    maj_method = "constant"
  )

  age_widths <- calc_widths(all_sites$ages, last = Inf)
  removal_rate <- (all_sites$cancer +
    all_sites$death -
    all_sites$cancer_death) /
    all_sites$pys
  expected_fracs <- ifelse(
    all_sites$cancer + all_sites$death - all_sites$cancer_death == 0,
    0,
    all_sites$cancer /
      (all_sites$cancer + all_sites$death - all_sites$cancer_death)
  )
  expected_contrib <- numeric(length(all_sites$ages))
  expected_s0 <- numeric(length(all_sites$ages))
  cum_removal <- 0
  for (i in seq_along(all_sites$ages)) {
    expected_s0[i] <- exp(-cum_removal)
    exp_term <- if (is.finite(age_widths[i])) {
      1 - exp(-age_widths[i] * removal_rate[i])
    } else {
      1
    }
    expected_contrib[i] <- expected_fracs[i] * expected_s0[i] * exp_term
    if (is.finite(age_widths[i])) {
      cum_removal <- cum_removal + age_widths[i] * removal_rate[i]
    }
  }

  expect_equal(res$ages, all_sites$ages)
  expect_equal(res$widths, age_widths)
  expect_equal(res$fracs, expected_fracs)
  expect_equal(res$rem_rates, removal_rate)
  expect_equal(res$S0ai, expected_s0)
  expect_equal(res$contrib_a, expected_contrib)
})

test_that("calc_ltr passes maj_method arguments to amp", {
  data("seer_example_data")
  all_sites <- seer_example_data[
    seer_example_data$site == "All" & seer_example_data$sex == 0,
  ]

  res_constant <- calc_ltr(
    ages = all_sites$ages,
    cancer = all_sites$cancer,
    cancer_death = all_sites$cancer_death,
    death = all_sites$death,
    pys = all_sites$pys,
    risk_func = "amp",
    maj_method = "constant",
    ci_method = "delta",
    age_start = 0,
    age_end = Inf,
    digits = 6
  )

  res_pmaj <- calc_ltr(
    ages = all_sites$ages,
    cancer = all_sites$cancer,
    cancer_death = all_sites$cancer_death,
    death = all_sites$death,
    pys = all_sites$pys,
    risk_func = "amp",
    maj_method = "pmaj",
    pmaj_sub_interval = 1,
    ci_method = "delta",
    age_start = 0,
    age_end = Inf,
    digits = 6
  )

  expect_s3_class(res_constant, "data.frame")
  expect_s3_class(res_pmaj, "data.frame")
  expect_named(res_constant, c("start", "end", "risk", "lower", "upper"))
  expect_named(res_pmaj, c("start", "end", "risk", "lower", "upper"))
})
