test_that("wun handles zero-rate closed intervals", {
  model <- wun(
    ages = c(0, 5),
    cancer = c(0, 10),
    cancer_death = c(0, 5),
    death = c(0, 20),
    pys = c(1000, 1000)
  )

  expect_equal(model$contrib_a[[1]], 0)
  expect_true(all(is.finite(model$contrib_a)))
  expect_true(all(is.finite(model$condi_p)))
})

test_that("wun supports one finite age interval", {
  model <- wun(
    ages = 0,
    cancer = 10,
    cancer_death = 2,
    death = 20,
    pys = 1000,
    last_age_widths = 5
  )

  expect_length(model$contrib_a, 1)
  expect_true(is.finite(model$contrib_a))
  expect_gt(model$contrib_a, 0)
})

test_that("wun validates surgery correction inputs", {
  args <- list(
    ages = c(0, 5),
    cancer = c(1, 2),
    cancer_death = c(0, 1),
    death = c(2, 3),
    pys = c(1000, 1000),
    correct_for_surgery = TRUE
  )

  expect_snapshot(error = TRUE, do.call(wun, args))
  expect_snapshot(
    error = TRUE,
    do.call(wun, c(args, list(H = c(1, 1), Ch = c(2, 0))))
  )
})

test_that("wun rejects an unidentified open interval", {
  expect_snapshot(
    error = TRUE,
    wun(
      ages = 0,
      cancer = 1,
      cancer_death = 0,
      death = 0,
      pys = 1000
    )
  )
})
