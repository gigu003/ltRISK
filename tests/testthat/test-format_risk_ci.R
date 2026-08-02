test_that("format_risk_ci formats risk and confidence intervals", {
  expect_equal(
    format_risk_ci(12.3, 10.1, 14.5),
    "12.3 (10.1, 14.5)"
  )

  expect_equal(
    format_risk_ci(12.3, 10.1, 14.5, show_conf_level = TRUE),
    "12.3 (95% CI 10.1, 14.5)"
  )
})

test_that("format_risk_ci controls displayed decimal places", {
  expect_equal(
    format_risk_ci(12.345, 10.123, 14.567, digits = 1),
    "12.3 (10.1, 14.6)"
  )
})

test_that("format_risk_ci handles vectors and missing values", {
  expect_equal(
    format_risk_ci(c(1, NA), c(0.5, 1), c(1.5, 2), digits = 1),
    c("1.0 (0.5, 1.5)", NA_character_)
  )

  expect_equal(
    format_risk_ci(c(1, NA), c(0.5, 1), c(1.5, 2), digits = 1, na = "missing"),
    c("1.0 (0.5, 1.5)", "missing")
  )
})

test_that("format_risk_ci adds a formatted column to data frames", {
  x <- data.frame(
    start = 0,
    end = Inf,
    risk = 12.345,
    lower = 10.123,
    upper = 14.567
  )

  out <- format_risk_ci(x, digits = 1)

  expect_s3_class(out, "data.frame")
  expect_named(out, c("start", "end", "risk", "lower", "upper", "risk_95ci"))
  expect_equal(out$risk_95ci, "12.3 (10.1, 14.6)")
})

test_that("format_risk_ci supports custom data frame columns", {
  x <- data.frame(
    estimate = 12.345,
    lcl = 10.123,
    ucl = 14.567
  )

  out <- format_risk_ci(
    x,
    risk_col = "estimate",
    lower_col = "lcl",
    upper_col = "ucl",
    name = "risk_ci",
    digits = 1
  )

  expect_equal(out$risk_ci, "12.3 (10.1, 14.6)")
})

test_that("format_risk_ci validates inputs", {
  expect_error(format_risk_ci(1, 0.5, c(1.5, 2)), "same length")
  expect_error(format_risk_ci(1, 0.5, 1.5, digits = -1), "non-negative integer")
  expect_error(format_risk_ci(1), "lower and upper")
  expect_error(
    format_risk_ci(data.frame(risk = 1, lower = 0.5)),
    "Missing columns"
  )
})

test_that("format_risk_ci inherits calculation digits", {
  data("seer_example_data")
  breast <- seer_example_data[seer_example_data$site == "Breast", ]

  for (digits in c(2L, 4L, 6L)) {
    risk <- calc_ltr_df(
      breast,
      ci_method = "delta",
      digits = digits
    )
    formatted <- format_risk_ci(risk)
    parts <- regmatches(
      formatted$risk_95ci,
      gregexpr("[0-9]+\\.[0-9]+", formatted$risk_95ci)
    )[[1]]
    expect_true(all(nchar(sub("^[^.]*\\.", "", parts)) == digits))
  }
})

test_that("explicit format digits override calculation digits", {
  data("seer_example_data")
  risk <- calc_ltr_df(
    seer_example_data,
    by = c("site", "sex"),
    ci_method = "delta",
    digits = 6
  )

  inherited <- format_risk_ci(risk)
  overridden <- format_risk_ci(risk, digits = 2)
  expect_match(inherited$risk_95ci[[1]], "\\.[0-9]{6} ")
  expect_match(overridden$risk_95ci[[1]], "\\.[0-9]{2} ")
})
