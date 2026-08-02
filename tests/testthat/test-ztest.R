test_that("ztest compares two calc_ltr results", {
  data("seer_example_data")
  all_sites <- seer_example_data[
    seer_example_data$site == "All" & seer_example_data$sex %in% c(1, 2),
  ]
  male <- all_sites[all_sites$sex == 1, ]
  female <- all_sites[all_sites$sex == 2, ]

  risk_male <- calc_ltr(
    ages = male$ages,
    cancer = male$cancer,
    cancer_death = male$cancer_death,
    death = male$death,
    pys = male$pys,
    risk_func = "devcan",
    maj_method = "constant",
    ci_method = "delta",
    age_start = c(0, 40),
    age_end = Inf,
    return_variance = TRUE,
    digits = 6
  )
  risk_female <- calc_ltr(
    ages = female$ages,
    cancer = female$cancer,
    cancer_death = female$cancer_death,
    death = female$death,
    pys = female$pys,
    risk_func = "devcan",
    maj_method = "constant",
    ci_method = "delta",
    age_start = c(0, 40),
    age_end = Inf,
    return_variance = TRUE,
    digits = 6
  )

  res <- ztest(risk_male, risk_female, digits = 10)

  expected_diff <- risk_male$risk - risk_female$risk
  expected_se <- sqrt(risk_male$variance + risk_female$variance)
  expected_z <- expected_diff / expected_se
  expected_p <- 2 * pnorm(-abs(expected_z))

  expect_named(
    res,
    c(
      "group_x",
      "group_y",
      "start",
      "end",
      "risk_x",
      "risk_y",
      "difference",
      "se",
      "z",
      "p",
      "lower",
      "upper"
    )
  )
  expect_equal(res$difference, round(expected_diff, 10))
  expect_equal(res$se, round(expected_se, 10))
  expect_equal(res$z, round(expected_z, 10))
  expect_equal(res$p, expected_p)
})

test_that("ztest compares grouped calc_ltr_df results", {
  data("seer_example_data")
  all_sites <- seer_example_data[
    seer_example_data$site == "All" & seer_example_data$sex %in% c(1, 2),
  ]

  risks <- calc_ltr_df(
    all_sites,
    by = "sex",
    ages = ages,
    cancer = cancer,
    cancer_death = cancer_death,
    death = death,
    pys = pys,
    risk_func = "devcan",
    maj_method = "constant",
    ci_method = "delta",
    age_start = c(0, 40),
    age_end = Inf,
    return_variance = TRUE,
    digits = 6
  )

  res <- ztest(risks, group = "sex", ref = 2, compare = 1, digits = 10)
  male <- risks[risks$sex == 1, ]
  female <- risks[risks$sex == 2, ]
  expected_diff <- male$risk - female$risk
  expected_se <- sqrt(male$variance + female$variance)

  expect_named(
    res,
    c(
      "group_compare",
      "group_ref",
      "start",
      "end",
      "risk_compare",
      "risk_ref",
      "difference",
      "se",
      "z",
      "p",
      "lower",
      "upper"
    )
  )
  expect_equal(res$group_compare, rep(1, nrow(res)))
  expect_equal(res$group_ref, rep(2, nrow(res)))
  expect_equal(res$difference, round(expected_diff, 10))
  expect_equal(res$se, round(expected_se, 10))
})

test_that("ztest validates required variance and matching age ranges", {
  x <- data.frame(start = 0, end = Inf, risk = 10)
  y <- data.frame(start = 0, end = Inf, risk = 12, variance = 0.1)
  expect_error(ztest(x, y), "Missing columns: variance")

  x2 <- data.frame(start = 0, end = 75, risk = 10, variance = 0.1)
  y2 <- data.frame(start = 0, end = 80, risk = 12, variance = 0.1)
  expect_error(ztest(x2, y2), "same start/end age ranges")
})

test_that("ztest rejects duplicate ranges and zero standard errors", {
  duplicate <- data.frame(
    start = c(0, 0),
    end = c(Inf, Inf),
    risk = c(1, 2),
    variance = c(0.1, 0.1)
  )
  valid <- data.frame(start = 0, end = Inf, risk = 1, variance = 0.1)
  zero <- data.frame(start = 0, end = Inf, risk = 1, variance = 0)

  expect_snapshot(error = TRUE, ztest(duplicate, valid))
  expect_snapshot(error = TRUE, ztest(zero, zero))
})

test_that("ztest validates grouped input", {
  x <- data.frame(
    sex = c(1, 2, 3),
    start = c(0, 0, 0),
    end = c(Inf, Inf, Inf),
    risk = c(10, 12, 13),
    variance = c(0.1, 0.1, 0.1)
  )

  expect_error(ztest(x), "group must be supplied")
  expect_error(ztest(x, group = "missing"), "group column not found")
  expect_error(ztest(x, group = "sex"), "ref and compare must be supplied")
  expect_error(
    ztest(x, group = "sex", ref = 1, compare = 1),
    "different groups"
  )
})

test_that("pairwise_ztest compares all group pairs", {
  x <- data.frame(
    group = rep(c("A", "B", "C"), each = 2),
    start = rep(c(0, 40), times = 3),
    end = rep(c(40, Inf), times = 3),
    risk = c(10, 20, 12, 24, 15, 28),
    variance = c(0.10, 0.20, 0.12, 0.22, 0.15, 0.25)
  )

  res <- pairwise_ztest(
    x,
    group = "group",
    p_adjust_method = "holm",
    digits = 10
  )

  expect_named(
    res,
    c(
      "group_compare",
      "group_ref",
      "start",
      "end",
      "risk_compare",
      "risk_ref",
      "difference",
      "se",
      "z",
      "p",
      "p_adjusted",
      "lower",
      "upper"
    )
  )
  expect_equal(nrow(res), 6)
  expect_equal(
    unique(paste(res$group_compare, res$group_ref, sep = "-")),
    c("B-A", "C-A", "C-B")
  )

  first <- res[1, ]
  expected_diff <- 12 - 10
  expected_se <- sqrt(0.12 + 0.10)
  expected_z <- expected_diff / expected_se
  expected_p <- 2 * pnorm(-abs(expected_z))
  expect_equal(first$difference, round(expected_diff, 10))
  expect_equal(first$se, round(expected_se, 10))
  expect_equal(first$z, round(expected_z, 10))
  expect_equal(first$p, expected_p)
  expect_equal(res$p_adjusted, p.adjust(res$p, method = "holm"))
})

test_that("pairwise_ztest compares groups against a reference", {
  x <- data.frame(
    group = rep(c("A", "B", "C"), each = 1),
    start = 0,
    end = Inf,
    risk = c(10, 12, 15),
    variance = c(0.10, 0.12, 0.15)
  )

  res <- pairwise_ztest(x, group = "group", ref = "A", p_adjust_method = "none")

  expect_equal(nrow(res), 2)
  expect_equal(res$group_ref, c("A", "A"))
  expect_equal(res$group_compare, c("B", "C"))
  expect_equal(res$p_adjusted, res$p)
  expect_equal(attr(res, "p.adjust.method"), "none")
})

test_that("pairwise_ztest validates grouped input", {
  one_group <- data.frame(
    group = "A",
    start = 0,
    end = Inf,
    risk = 10,
    variance = 0.1
  )
  missing_variance <- data.frame(
    group = c("A", "B"),
    start = c(0, 0),
    end = c(Inf, Inf),
    risk = c(10, 12)
  )

  expect_error(
    pairwise_ztest(one_group, group = "group"),
    "at least two values"
  )
  expect_error(
    pairwise_ztest(one_group, group = "missing"),
    "group column not found"
  )
  expect_error(
    pairwise_ztest(missing_variance, group = "group"),
    "Missing columns: variance"
  )
  expect_error(
    pairwise_ztest(one_group, group = "group", ref = "B"),
    "at least two values"
  )
})
