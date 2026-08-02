test_that("pairwise age ranges return positional intervals", {
  data("seer_example_data")
  breast <- seer_example_data[seer_example_data$site == "Breast", ]
  starts <- c(0, 40, 50, 60, 70, 80)
  ends <- c(40, 50, 60, 70, 80, Inf)

  pairwise <- calc_ltr_df(
    breast,
    age_start = starts,
    age_end = ends,
    age_combine = "pairwise",
    ci_method = "gamma"
  )
  cross <- calc_ltr_df(
    breast,
    age_start = starts,
    age_end = ends,
    age_combine = "cross",
    ci_method = "none"
  )

  expect_equal(pairwise$start, starts)
  expect_equal(pairwise$end, ends)
  expect_equal(nrow(pairwise), 6L)
  expect_equal(nrow(cross), 21L)
})

test_that("pairwise ranges validate lengths and ordering", {
  data("seer_example_data")
  breast <- seer_example_data[seer_example_data$site == "Breast", ]

  expect_error(
    calc_ltr_df(
      breast,
      age_start = c(0, 40),
      age_end = c(50, 60, Inf),
      age_combine = "pairwise",
      ci_method = "none"
    ),
    "equal lengths"
  )
  expect_error(
    calc_ltr_df(
      breast,
      age_start = c(0, 50),
      age_end = c(40, 40),
      age_combine = "pairwise",
      ci_method = "none"
    ),
    "greater than"
  )
})

test_that("calc_ltr validates ages and display arguments early", {
  data("seer_example_data")
  breast <- seer_example_data[seer_example_data$site == "Breast", ]
  duplicate <- breast
  duplicate$ages[[2]] <- duplicate$ages[[1]]

  expect_error(
    calc_ltr_df(duplicate, ci_method = "none"),
    "strictly increasing"
  )
  for (bad_digits in list(-1, 2.5, NA_real_, "2")) {
    expect_error(
      calc_ltr_df(breast, digits = bad_digits, ci_method = "none"),
      "non-negative whole number"
    )
  }
  expect_error(
    calc_ltr_df(breast, multiplier = -1, ci_method = "none"),
    "multiplier"
  )
  expect_error(
    calc_ltr_df(breast, age_start = NA_real_, ci_method = "none"),
    "age_start"
  )
})

test_that("constant age models explain unavailable boundaries", {
  data("seer_example_data")
  breast <- seer_example_data[seer_example_data$site == "Breast", ]
  expect_error(
    calc_ltr_df(
      breast,
      maj_method = "constant",
      age_start = 42,
      ci_method = "none"
    ),
    "pmaj"
  )
})
