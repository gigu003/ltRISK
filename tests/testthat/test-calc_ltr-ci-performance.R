test_that("gamma PMAJ results remain numerically stable", {
  data("seer_example_data")
  breast <- seer_example_data[seer_example_data$site == "Breast", ]

  res <- calc_ltr(
    ages = breast$ages,
    cancer = breast$cancer,
    cancer_death = breast$cancer_death,
    death = breast$death,
    pys = breast$pys,
    age_start = c(0, 30, 50, 70),
    age_end = c(30, 50, 70, Inf),
    ci_method = "gamma",
    maj_method = "pmaj",
    digits = 10
  )

  expect_equal(
    res$risk,
    c(
      0.0570297836,
      1.9432935283,
      7.8097416065,
      13.2795824322,
      1.9163381458,
      7.8763202872,
      13.4333717095,
      6.2367371880,
      12.0518333475,
      7.2714887582
    )
  )
  expect_equal(
    res$upper,
    c(
      0.0615412279,
      1.9708112239,
      7.8814471904,
      13.3829639086,
      1.9436401048,
      7.9489079888,
      13.5381818834,
      6.3051744146,
      12.1564859893,
      7.3648819537
    )
  )
})

test_that("fast Wun evaluator agrees with the public model", {
  d <- seer_example_data[
    seer_example_data$site == "All" & seer_example_data$sex == 0,
  ]
  model <- wun(d$ages, d$cancer, d$cancer_death, d$death, d$pys)
  expected <- get_risk(
    model,
    c(0, 30, 50, 70),
    c(30, 50, 70, Inf),
    age_combine = "pairwise"
  )
  actual <- wun_risk_fast(
    d$ages,
    d$cancer,
    d$cancer_death,
    d$death,
    d$pys,
    c(0, 30, 50, 70),
    c(30, 50, 70, Inf)
  )

  expect_equal(actual, expected, tolerance = 1e-14)
})

test_that("fast cumulative evaluator matches the public model", {
  d <- seer_example_data[
    seer_example_data$site == "All" & seer_example_data$sex == 0,
  ]
  starts <- c(0, 30, 50)
  ends <- c(85, 85, 70)

  for (type in c("developing", "dying")) {
    for (maj_method in c("constant", "pmaj")) {
      model <- cumulative(
        d$ages,
        d$cancer,
        d$cancer_death,
        d$death,
        d$pys,
        type = type,
        maj_method = maj_method
      )
      expected <- get_risk(model, starts, ends, age_combine = "pairwise")
      actual <- cumulative_risk_fast(
        d$ages,
        d$cancer,
        d$cancer_death,
        d$death,
        d$pys,
        starts,
        ends,
        type = type,
        maj_method = maj_method
      )
      expect_equal(actual, unname(expected), tolerance = 1e-14)
    }
  }
})

test_that("fast cumulative evaluator handles finite final intervals", {
  args <- list(
    ages = c(0, 5, 10),
    cancer = c(1, 2, 3),
    cancer_death = c(0, 1, 1),
    death = c(2, 3, 4),
    pys = c(1000, 1000, 1000),
    last_age_widths = 5,
    maj_method = "pmaj"
  )
  model <- do.call(cumulative, args)
  terminal_age <- tail(model$ages, 1) + tail(model$widths, 1)
  expected <- get_risk(
    model,
    c(0, 5),
    rep(terminal_age, 2),
    age_combine = "pairwise"
  )
  actual <- do.call(
    cumulative_risk_fast,
    c(args, list(age_start = c(0, 5), age_end = rep(terminal_age, 2)))
  )
  expect_equal(actual, unname(expected), tolerance = 1e-14)
})

test_that("PMAJ grid preserves short remainder intervals", {
  grid <- pmaj_grid(
    ages = c(0, 5, 10),
    rate_hat = c(1, 2, 3),
    pmaj_sub_interval = 3
  )

  expect_equal(grid$ages, c(0, 3, 5, 8, 10, 12.5))
  expect_equal(grid$widths, c(3, 2, 3, 2, 2.5, Inf))
  expect_length(grid$rate, length(grid$ages))
  expect_equal(drop(grid$design %*% c(1, 2, 3)), grid$rate)
})

test_that("gamma confidence interval handles zero risk", {
  res <- calc_ltr(
    ages = c(0, 5),
    cancer = c(0, 0),
    cancer_death = c(0, 0),
    death = c(1, 1),
    pys = c(1000, 1000),
    age_start = 0,
    age_end = Inf,
    maj_method = "constant",
    ci_method = "gamma"
  )

  expect_equal(res$risk, 0)
  expect_equal(res$lower, 0)
  expect_true(is.finite(res$upper))
  expect_gte(res$upper, 0)
})

test_that("calc_ltr validates core registry inputs", {
  args <- list(
    ages = c(0, 5),
    cancer = c(1, 2),
    cancer_death = c(0, 1),
    death = c(2, 3),
    pys = c(1000, 1000),
    age_start = 0,
    age_end = Inf,
    maj_method = "constant",
    ci_method = "delta"
  )

  expect_error(
    do.call(calc_ltr, modifyList(args, list(pys = c(1000, 0)))),
    "positive"
  )
  expect_error(
    do.call(calc_ltr, modifyList(args, list(cancer_death = c(3, 1)))),
    "must not exceed"
  )
  expect_error(
    do.call(calc_ltr, modifyList(args, list(alpha = 1))),
    "between 0 and 1"
  )
})

test_that("ci_method none skips confidence interval columns", {
  data("seer_example_data")
  breast <- seer_example_data[seer_example_data$site == "Breast", ]

  res <- calc_ltr(
    ages = breast$ages,
    cancer = breast$cancer,
    cancer_death = breast$cancer_death,
    death = breast$death,
    pys = breast$pys,
    age_start = c(0, 50),
    age_end = Inf,
    maj_method = "constant",
    ci_method = "none"
  )

  expect_named(res, c("start", "end", "risk"))
  expect_error(
    calc_ltr(
      breast$ages,
      breast$cancer,
      breast$cancer_death,
      breast$death,
      breast$pys,
      ci_method = "none",
      return_variance = TRUE
    ),
    "cannot be TRUE"
  )
})

test_that("analytic cumulative variance agrees with finite differences", {
  data("seer_example_data")
  d <- seer_example_data[seer_example_data$site == "Breast", ]
  args <- list(
    ages = d$ages,
    cancer = d$cancer,
    cancer_death = d$cancer_death,
    death = d$death,
    pys = d$pys,
    risk_func = "cumulative",
    maj_method = "constant",
    ci_method = "delta",
    age_start = c(0, 30, 50),
    age_end = 85,
    multiplier = 1,
    return_variance = TRUE
  )

  analytic <- do.call(calc_ltr, c(args, list(variance_method = "analytic")))
  finite <- do.call(
    calc_ltr,
    c(args, list(variance_method = "finite_difference"))
  )

  expect_equal(analytic$variance, finite$variance, tolerance = 1e-5)
})

test_that("analytic DevCan variance agrees with finite differences", {
  data("seer_example_data")
  d <- seer_example_data[seer_example_data$site == "Breast", ]
  args <- list(
    ages = d$ages,
    cancer = d$cancer,
    cancer_death = d$cancer_death,
    death = d$death,
    pys = d$pys,
    risk_func = "devcan",
    maj_method = "constant",
    ci_method = "delta",
    age_start = c(0, 30, 50, 70),
    age_end = c(30, 50, 70, Inf),
    multiplier = 1,
    return_variance = TRUE
  )

  analytic <- do.call(calc_ltr, c(args, list(variance_method = "analytic")))
  finite <- do.call(
    calc_ltr,
    c(args, list(variance_method = "finite_difference"))
  )

  expect_equal(analytic$variance, finite$variance, tolerance = 2e-5)
})

test_that("analytic PMAJ variance agrees with finite differences", {
  data("seer_example_data")
  d <- seer_example_data[seer_example_data$site == "Breast", ]

  args <- list(
    ages = d$ages,
    cancer = d$cancer,
    cancer_death = d$cancer_death,
    death = d$death,
    pys = d$pys,
    age_start = c(0, 30, 50, 70),
    age_end = c(30, 50, 70, Inf),
    maj_method = "pmaj",
    ci_method = "delta",
    multiplier = 1,
    return_variance = TRUE
  )
  analytic <- do.call(calc_ltr, c(args, list(variance_method = "analytic")))
  finite <- do.call(
    calc_ltr,
    c(args, list(variance_method = "finite_difference"))
  )

  expect_equal(analytic$variance, finite$variance, tolerance = 2e-5)

  cumulative_analytic <- do.call(
    calc_ltr,
    c(
      args,
      list(
        risk_func = "cumulative",
        variance_method = "analytic"
      )
    )
  )
  cumulative_finite <- do.call(
    calc_ltr,
    c(
      args,
      list(
        risk_func = "cumulative",
        variance_method = "finite_difference"
      )
    )
  )
  expect_equal(
    cumulative_analytic$variance,
    cumulative_finite$variance,
    tolerance = 2e-5
  )
})

test_that("fast DevCan evaluator matches the model object", {
  data("seer_example_data")
  d <- seer_example_data[seer_example_data$site == "Breast", ]
  starts <- c(0, 30, 50, 70)
  ends <- c(30, 50, 70, Inf)

  for (maj_method in c("constant", "pmaj")) {
    for (type in c("developing", "dying")) {
      model <- devcan(
        d$ages,
        d$cancer,
        d$death,
        d$cancer_death,
        d$pys,
        type = type,
        maj_method = maj_method
      )
      expected <- get_risk(model, starts, ends, age_combine = "pairwise")
      actual <- devcan_risk_fast(
        d$ages,
        d$cancer,
        d$cancer_death,
        d$death,
        d$pys,
        starts,
        ends,
        type = type,
        maj_method = maj_method
      )
      expect_equal(actual, expected, tolerance = 1e-14)
    }
  }
})

test_that("analytic AMP variance agrees with finite differences", {
  data("seer_example_data")
  d <- seer_example_data[seer_example_data$site == "Breast", ]
  args <- list(
    ages = d$ages,
    cancer = d$cancer,
    cancer_death = d$cancer_death,
    death = d$death,
    pys = d$pys,
    age_start = c(0, 30, 50, 70),
    age_end = c(30, 50, 70, Inf),
    risk_func = "amp",
    ci_method = "delta",
    multiplier = 1,
    return_variance = TRUE
  )

  for (maj_method in c("constant", "pmaj")) {
    for (type in c("developing", "dying")) {
      analytic <- do.call(
        calc_ltr,
        c(
          args,
          list(
            maj_method = maj_method,
            type = type,
            variance_method = "analytic"
          )
        )
      )
      finite <- do.call(
        calc_ltr,
        c(
          args,
          list(
            maj_method = maj_method,
            type = type,
            variance_method = "finite_difference"
          )
        )
      )
      expect_equal(analytic$variance, finite$variance, tolerance = 2e-5)
    }
  }
})

test_that("fast AMP evaluator matches the model object", {
  data("seer_example_data")
  d <- seer_example_data[seer_example_data$site == "Breast", ]
  starts <- c(0, 30, 50, 70)
  ends <- c(30, 50, 70, Inf)

  for (maj_method in c("constant", "pmaj")) {
    for (type in c("developing", "dying")) {
      model <- amp(
        d$ages,
        d$cancer,
        d$cancer_death,
        d$death,
        d$pys,
        type = type,
        maj_method = maj_method
      )
      expect_equal(
        amp_risk_fast(
          d$ages,
          d$cancer,
          d$cancer_death,
          d$death,
          d$pys,
          starts,
          ends,
          type = type,
          maj_method = maj_method
        ),
        get_risk(model, starts, ends, age_combine = "pairwise"),
        tolerance = 1e-14
      )
    }
  }
})

test_that("analytic Wun variance agrees with finite differences", {
  data("seer_example_data")
  d <- seer_example_data[
    seer_example_data$site == "All" & seer_example_data$sex == 0,
  ]
  args <- list(
    ages = d$ages,
    cancer = d$cancer,
    cancer_death = d$cancer_death,
    death = d$death,
    pys = d$pys,
    risk_func = "wun",
    ci_method = "delta",
    age_start = c(0, 30, 50, 70),
    age_end = c(30, 50, 70, Inf),
    age_combine = "pairwise",
    multiplier = 1,
    return_variance = TRUE
  )

  for (type in c("developing", "dying")) {
    analytic <- do.call(
      calc_ltr,
      c(args, list(type = type, variance_method = "analytic"))
    )
    finite <- do.call(
      calc_ltr,
      c(args, list(type = type, variance_method = "finite_difference"))
    )
    expect_equal(analytic$variance, finite$variance, tolerance = 2e-6)
  }
})

test_that("analytic Wun gamma intervals agree with finite differences", {
  d <- seer_example_data[
    seer_example_data$site == "All" & seer_example_data$sex == 0,
  ]
  args <- list(
    ages = d$ages,
    cancer = d$cancer,
    cancer_death = d$cancer_death,
    death = d$death,
    pys = d$pys,
    risk_func = "wun",
    ci_method = "gamma",
    age_start = c(0, 30, 50, 70),
    age_end = c(30, 50, 70, Inf),
    age_combine = "pairwise",
    digits = 12
  )
  analytic <- do.call(
    calc_ltr,
    c(args, list(variance_method = "analytic"))
  )
  finite <- do.call(
    calc_ltr,
    c(args, list(variance_method = "finite_difference"))
  )

  expect_equal(analytic$risk, finite$risk)
  expect_equal(analytic$lower, finite$lower, tolerance = 2e-8)
  expect_equal(analytic$upper, finite$upper, tolerance = 2e-8)
})

test_that("analytic Wun variance supports finite final intervals", {
  args <- list(
    ages = c(0, 5, 10),
    cancer = c(3, 8, 14),
    cancer_death = c(1, 2, 4),
    death = c(10, 20, 35),
    pys = c(10000, 9000, 8000),
    risk_func = "wun",
    ci_method = "delta",
    last_age_widths = 5,
    age_start = c(0, 5),
    age_end = c(10, 15),
    age_combine = "pairwise",
    multiplier = 1,
    return_variance = TRUE
  )
  analytic <- do.call(
    calc_ltr,
    c(args, list(variance_method = "analytic"))
  )
  finite <- do.call(
    calc_ltr,
    c(args, list(variance_method = "finite_difference"))
  )
  expect_equal(analytic$variance, finite$variance, tolerance = 2e-5)
})

test_that("Wun surgery correction falls back from auto variance", {
  d <- seer_example_data[
    seer_example_data$site == "All" & seer_example_data$sex == 0,
  ]
  H <- pmax(d$cancer + 5, 10)
  Ch <- pmin(d$cancer, H)
  args <- list(
    ages = d$ages,
    cancer = d$cancer,
    cancer_death = d$cancer_death,
    death = d$death,
    pys = d$pys,
    risk_func = "wun",
    ci_method = "delta",
    correct_for_surgery = TRUE,
    H = H,
    Ch = Ch,
    multiplier = 1,
    return_variance = TRUE
  )

  auto <- do.call(calc_ltr, c(args, list(variance_method = "auto")))
  finite <- do.call(
    calc_ltr,
    c(args, list(variance_method = "finite_difference"))
  )
  expect_equal(auto$variance, finite$variance)

  expect_error(
    do.call(calc_ltr, c(args, list(variance_method = "analytic"))),
    "not available"
  )
})

test_that("delta zero-count correction agrees across variance engines", {
  args <- list(
    ages = c(0, 5, 10),
    cancer = c(0, 2, 0),
    cancer_death = c(0, 1, 1),
    death = c(3, 4, 5),
    pys = c(10000, 9000, 8000),
    age_start = 0,
    age_end = 15,
    risk_func = "amp",
    last_age_widths = 5,
    maj_method = "constant",
    ci_method = "delta",
    multiplier = 1,
    return_variance = TRUE
  )

  analytic <- do.call(
    calc_ltr,
    c(args, list(variance_method = "analytic"))
  )
  finite <- do.call(
    calc_ltr,
    c(args, list(variance_method = "finite_difference"))
  )

  expect_gt(analytic$variance, 0)
  expect_equal(analytic$variance, finite$variance, tolerance = 2e-5)
})

test_that("gamma preserves a truly degenerate zero variance", {
  res <- calc_ltr(
    ages = c(0, 5),
    cancer = c(0, 0),
    cancer_death = c(0, 0),
    death = c(2, 2),
    pys = c(1000, 1000),
    age_start = 0,
    age_end = Inf,
    risk_func = "cumulative",
    maj_method = "constant",
    ci_method = "gamma",
    return_variance = TRUE
  )

  expect_equal(res$risk, 0)
  expect_equal(res$lower, 0)
  expect_gt(res$upper, 0)
  expect_equal(res$variance, 0)
  expect_equal(res$se, 0)
})

test_that("finite last age widths affect AMP and cumulative risks", {
  args <- list(
    ages = c(0, 5),
    cancer = c(1, 10),
    cancer_death = c(0, 1),
    death = c(2, 3),
    pys = c(1000, 1000),
    age_start = 0,
    age_end = 10,
    maj_method = "constant",
    ci_method = "none"
  )

  for (risk_func in c("amp", "cumulative")) {
    finite <- do.call(
      calc_ltr,
      c(args, list(risk_func = risk_func, last_age_widths = 5))
    )
    expect_true(is.finite(finite$risk))
    expect_gt(finite$risk, 0)
  }
})
