test_that("trend_test detects weighted linear trend with numeric group", {
  x <- data.frame(
    period = rep(1:4, 2),
    start = rep(c(0, 40), each = 4),
    end = Inf,
    risk = c(10, 12, 14, 16, 20, 23, 26, 29),
    variance = rep(0.25, 8)
  )

  res <- trend_test(x, group = "period", digits = 10)

  expect_equal(nrow(res), 2)
  expect_equal(res$n_groups, c(4, 4))
  expect_equal(res$slope, c(2, 3))
  expect_true(all(res$p < 0.05))
})

test_that("trend_test supports named explicit scores", {
  x <- data.frame(
    level = rep(c("low", "medium", "high"), 2),
    start = rep(c(0, 50), each = 3),
    end = Inf,
    risk = c(10, 13, 19, 20, 22, 30),
    variance = rep(0.5, 6)
  )

  res <- trend_test(
    x,
    group = "level",
    score = c(low = 1, medium = 2, high = 4),
    digits = 10
  )

  expected_first <- coef(lm(
    risk ~ score,
    data = data.frame(risk = c(10, 13, 19), score = c(1, 2, 4)),
    weights = rep(1 / 0.5, 3)
  ))[["score"]]

  expect_equal(res$slope[1], round(unname(expected_first), 10))
})

test_that("trend_test warns for character group without score", {
  x <- data.frame(
    level = c("low", "medium", "high"),
    start = 0,
    end = Inf,
    risk = c(10, 12, 14),
    variance = rep(0.25, 3)
  )

  expect_warning(
    trend_test(x, group = "level"),
    "Character group values are scored by order of first appearance"
  )
})

test_that("trend_test validates inputs", {
  x_missing <- data.frame(group = 1:3, start = 0, end = Inf, risk = 1:3)
  expect_error(
    trend_test(x_missing, group = "group"),
    "Missing columns: variance"
  )

  x_two <- data.frame(
    group = 1:2,
    start = 0,
    end = Inf,
    risk = c(1, 2),
    variance = c(0.1, 0.1)
  )
  expect_error(trend_test(x_two, group = "group"), "at least three groups")

  x_bad_var <- data.frame(
    group = 1:3,
    start = 0,
    end = Inf,
    risk = c(1, 2, 3),
    variance = c(0.1, 0, 0.1)
  )
  expect_error(
    trend_test(x_bad_var, group = "group"),
    "variance must be positive"
  )

  x_score <- data.frame(
    group = c("a", "b", "c"),
    start = 0,
    end = Inf,
    risk = c(1, 2, 3),
    variance = rep(0.1, 3)
  )
  expect_error(
    trend_test(x_score, group = "group", score = c(a = 1, b = 2)),
    "score is missing values"
  )
})

test_that("trend_test rejects an unidentified score", {
  x <- data.frame(
    group = 1:3,
    start = 0,
    end = Inf,
    risk = 1:3,
    variance = rep(0.1, 3)
  )
  expect_snapshot(error = TRUE, trend_test(x, "group", score = rep(1, 3)))
})
