test_that("DevCan reproduces Fay 2003 Table II", {
  published <- utils::read.csv(
    testthat::test_path("fixtures", "fay2003-table-ii.csv"),
    na.strings = character()
  )
  published$end[published$end == "Inf"] <- Inf
  published$end <- as.numeric(published$end)
  d <- seer_example_data[seer_example_data$site == "Breast", ]

  actual <- do.call(
    rbind,
    lapply(unique(published$ci_method), function(method) {
      expected <- published[published$ci_method == method, ]
      result <- calc_ltr(
        ages = d$ages,
        cancer = d$cancer,
        cancer_death = d$cancer_death,
        death = d$death,
        pys = d$pys,
        risk_func = "devcan",
        maj_method = "constant",
        ci_method = method,
        age_start = expected$start,
        age_end = expected$end,
        age_combine = "pairwise",
        digits = 4
      )
      data.frame(ci_method = method, result, row.names = NULL)
    })
  )

  expect_equal(actual, published, tolerance = 5e-5)
})

test_that("DevCan Gamma reproduces the SEER 21 all-sites table", {
  expected <- utils::read.csv(
    testthat::test_path("fixtures", "seer-devcan-all-gamma.csv"),
    na.strings = character()
  )
  d <- seer_example_data[
    seer_example_data$site == "All" & seer_example_data$sex == 0,
  ]
  boundaries <- c(0, seq(5, 90, 5))

  actual <- calc_ltr(
    ages = d$ages,
    cancer = d$cancer,
    cancer_death = d$cancer_death,
    death = d$death,
    pys = d$pys,
    risk_func = "devcan",
    maj_method = "pmaj",
    ci_method = "gamma",
    age_start = boundaries,
    age_end = c(seq(5, 90, 5), Inf),
    age_combine = "cross",
    digits = 4
  )
  actual <- actual[order(actual$start, actual$end), ]
  expected <- expected[order(expected$start, expected$end), ]

  expect_equal(nrow(actual), 190L)
  expect_identical(anyDuplicated(actual[c("start", "end")]), 0L)
  expect_equal(actual[c("start", "end")], expected[c("start", "end")])
  expect_equal(round(actual$risk, 2), expected$risk, tolerance = 5e-9)
  expect_equal(actual$lower, expected$lower, tolerance = 2.1e-4)
  expect_equal(actual$upper, expected$upper, tolerance = 2.1e-4)
  expect_true(all(actual$lower <= actual$risk & actual$risk <= actual$upper))
})

test_that("gamma candidates preserve the other-death boundary", {
  args <- list(
    ages = c(0, 5),
    cancer = c(0, 1),
    cancer_death = c(0, 1),
    death = c(0, 1),
    pys = c(10000, 10000),
    age_start = 0,
    age_end = Inf,
    maj_method = "constant",
    ci_method = "gamma"
  )

  expect_no_error(result <- do.call(calc_ltr, args))
  expect_true(all(is.finite(unlist(result[c("risk", "lower", "upper")]))))
  expect_gte(result$lower, 0)
  expect_gte(result$upper, result$lower)
})
