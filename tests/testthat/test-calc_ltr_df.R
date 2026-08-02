test_that("calc_ltr keeps vector interfaces compatible", {
  data("seer_example_data")
  breast <- seer_example_data[seer_example_data$site == "Breast", ]

  res_named <- calc_ltr(
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

  res_positional <- calc_ltr(
    breast$ages,
    breast$cancer,
    breast$cancer_death,
    breast$death,
    breast$pys,
    maj_method = "constant",
    ci_method = "delta",
    age_start = 0,
    age_end = Inf,
    digits = 6
  )

  expect_equal(res_positional, res_named)
})

test_that("calc_ltr_df matches calc_ltr vector interface", {
  data("seer_example_data")
  breast <- seer_example_data[seer_example_data$site == "Breast", ]

  res_vec <- calc_ltr(
    ages = breast$ages,
    cancer = breast$cancer,
    cancer_death = breast$cancer_death,
    death = breast$death,
    pys = breast$pys,
    maj_method = "constant",
    ci_method = "delta",
    age_start = c(0, 30),
    age_end = Inf,
    digits = 6
  )

  res_df <- calc_ltr_df(
    breast,
    ages = "ages",
    cancer = "cancer",
    cancer_death = "cancer_death",
    death = "death",
    pys = "pys",
    maj_method = "constant",
    ci_method = "delta",
    age_start = c(0, 30),
    age_end = Inf,
    digits = 6
  )

  expect_equal(res_df, res_vec)
})

test_that("calc_ltr_df supports bare column names", {
  data("seer_example_data")
  breast <- seer_example_data[seer_example_data$site == "Breast", ]

  res_chr <- calc_ltr_df(
    breast,
    ages = "ages",
    cancer = "cancer",
    cancer_death = "cancer_death",
    death = "death",
    pys = "pys",
    maj_method = "constant",
    ci_method = "delta",
    age_start = 0,
    age_end = Inf,
    digits = 6
  )

  res_bare <- calc_ltr_df(
    breast,
    ages = ages,
    cancer = cancer,
    cancer_death = cancer_death,
    death = death,
    pys = pys,
    maj_method = "constant",
    ci_method = "delta",
    age_start = 0,
    age_end = Inf,
    digits = 6
  )

  expect_equal(res_bare, res_chr)
})

test_that("calc_ltr_df supports character variables", {
  data("seer_example_data")
  breast <- seer_example_data[seer_example_data$site == "Breast", ]
  age_col <- "ages"
  cancer_col <- "cancer"

  res <- calc_ltr_df(
    breast,
    ages = age_col,
    cancer = cancer_col,
    cancer_death = "cancer_death",
    death = "death",
    pys = "pys",
    maj_method = "constant",
    ci_method = "delta",
    age_start = 0,
    age_end = Inf,
    digits = 6
  )

  expect_s3_class(res, "data.frame")
  expect_named(res, c("start", "end", "risk", "lower", "upper"))
})

test_that("calc_ltr_df reports missing columns", {
  data("seer_example_data")
  breast <- seer_example_data[seer_example_data$site == "Breast", ]

  expect_error(
    calc_ltr_df(
      breast,
      ages = "missing_age",
      cancer = "cancer",
      cancer_death = "cancer_death",
      death = "death",
      pys = "pys",
      maj_method = "constant",
      ci_method = "delta"
    ),
    "Missing columns in data: missing_age"
  )
})

test_that("calc_ltr_df calculates risks by explicit groups", {
  data("seer_example_data")

  res <- calc_ltr_df(
    seer_example_data,
    by = c("site", "sex"),
    ages = ages,
    cancer = cancer,
    cancer_death = cancer_death,
    death = death,
    pys = pys,
    maj_method = "constant",
    ci_method = "delta",
    age_start = 0,
    age_end = Inf,
    digits = 6
  )

  expect_s3_class(res, "data.frame")
  expect_true("site" %in% names(res))
  expect_equal(sort(unique(res$site)), sort(unique(seer_example_data$site)))
  expect_named(res, c("site", "sex", "start", "end", "risk", "lower", "upper"))
})

test_that("calc_ltr_df detects dplyr grouped data frames", {
  skip_if_not_installed("dplyr")
  data("seer_example_data")

  grouped <- dplyr::group_by(seer_example_data, site, sex)
  res_grouped <- calc_ltr_df(
    grouped,
    ages = ages,
    cancer = cancer,
    cancer_death = cancer_death,
    death = death,
    pys = pys,
    maj_method = "constant",
    ci_method = "delta",
    age_start = 0,
    age_end = Inf,
    digits = 6
  )

  res_by <- calc_ltr_df(
    seer_example_data,
    by = c("site", "sex"),
    ages = ages,
    cancer = cancer,
    cancer_death = cancer_death,
    death = death,
    pys = pys,
    maj_method = "constant",
    ci_method = "delta",
    age_start = 0,
    age_end = Inf,
    digits = 6
  )

  expect_equal(res_grouped, res_by)
})

test_that("calc_ltr_df reports missing grouping columns", {
  data("seer_example_data")

  expect_error(
    calc_ltr_df(
      seer_example_data,
      by = "missing_group",
      maj_method = "constant",
      ci_method = "delta"
    ),
    "Missing grouping columns in data: missing_group"
  )
})

test_that("calc_ltr_df parallel groups match serial groups", {
  skip_on_cran()
  data("seer_example_data")
  d <- do.call(
    rbind,
    lapply(seq_len(2), function(year) {
      out <- seer_example_data
      out$type <- "country"
      out$cate <- paste0("cate", year)
      out$cancers <- out$site
      out$sex_label <- if (year == 1) "Female" else "Male"
      out$year <- 2020 + year
      out
    })
  )
  by <- c("type", "cate", "cancers", "sex", "sex_label", "year")
  args <- list(
    data = d,
    ages = "ages",
    cancer = "cancer",
    cancer_death = "cancer_death",
    death = "death",
    pys = "pys",
    by = by,
    risk_func = "amp",
    age_start = c(0, 40, 50, 60, 70, 80),
    age_end = Inf,
    type = "developing",
    digits = 2
  )

  serial <- do.call(calc_ltr_df, args)
  parallel <- do.call(calc_ltr_df, c(args, list(parallel = TRUE, workers = 2)))

  expect_identical(parallel, serial)
  expect_identical(attr(parallel, "risk_digits"), 2L)
  expect_identical(
    names(parallel),
    c(by, "start", "end", "risk", "lower", "upper")
  )
  expect_equal(nrow(parallel), 6 * length(unique(interaction(d[by]))))
  expect_s3_class(format_risk_ci(parallel), "data.frame")
})

test_that("calc_ltr_df validates parallel settings", {
  data("seer_example_data")
  expect_error(calc_ltr_df(NULL), "data must be a data frame")
  expect_error(
    calc_ltr_df(seer_example_data, by = c("site", "sex"), parallel = NA),
    "parallel must"
  )
  expect_error(
    calc_ltr_df(
      seer_example_data,
      by = c("site", "sex"),
      parallel = TRUE,
      workers = 0
    ),
    "positive whole number"
  )
  expect_error(
    calc_ltr_df(seer_example_data, by = 1, ci_method = "none"),
    "by must be a character vector"
  )
  expect_error(
    calc_ltr_df(
      seer_example_data,
      ages = seer_example_data$ages,
      ci_method = "none"
    ),
    "column specifications"
  )
})

test_that("calc_ltr_df handles one-worker and cache policies", {
  data("seer_example_data")
  d <- seer_example_data[seer_example_data$site == "All", ]
  args <- list(
    data = d,
    by = "sex",
    ci_method = "none",
    maj_method = "constant"
  )

  serial <- do.call(calc_ltr_df, args)
  one_worker <- do.call(
    calc_ltr_df,
    c(args, list(parallel = TRUE, workers = 1, cache = "clear"))
  )
  no_cache <- do.call(calc_ltr_df, c(args, list(cache = "none")))

  expect_identical(one_worker, serial)
  expect_identical(no_cache, serial)
  expect_true(all(ltr_cache_info()$entries == 0L))
})

test_that("internal input helpers diagnose malformed values", {
  expect_error(ltRISK:::calc_widths(numeric()), "at least one element")
  expect_equal(ltRISK:::calc_widths(5, last = 10), 10)
  expect_equal(ltRISK:::calc_widths(c(0, 1, 5), last = 7), c(1, 4, 7))

  valid <- list(
    ages = c(0, 5),
    cancer = c(1, 2),
    cancer_death = c(0, 1),
    death = c(1, 2),
    pys = c(100, 100),
    type = "developing"
  )
  expect_error(
    do.call(
      ltRISK:::validate_model_inputs,
      c(valid[names(valid) != "type"], list(type = NA_character_))
    ),
    "type must"
  )
  expect_error(
    do.call(ltRISK:::validate_model_inputs, c(valid, list(last_age_widths = 0))),
    "positive number"
  )
  expect_error(
    ltRISK:::validate_rate_inputs(c(0, 5), c(0.1, Inf)),
    "finite values"
  )
  expect_true(ltRISK:::validate_rate_inputs(c(0, 5), c(0, 0.1)))
})

test_that("parallel group errors retain the group label", {
  skip_on_cran()
  data("seer_example_data")
  bad <- seer_example_data
  bad$pys[bad$site == "Breast"] <- 0

  expect_error(
    calc_ltr_df(
      bad,
      by = c("site", "sex"),
      parallel = TRUE,
      workers = 2,
      ci_method = "none"
    ),
    "pys must contain positive values",
    fixed = TRUE
  )
})

test_that("calc_ltr_df can reuse an external cluster", {
  skip_on_cran()
  data("seer_example_data")
  cluster <- parallel::makePSOCKcluster(2)
  on.exit(parallel::stopCluster(cluster), add = TRUE)

  serial <- calc_ltr_df(
    seer_example_data,
    by = c("site", "sex"),
    ci_method = "none"
  )
  first <- calc_ltr_df(
    seer_example_data,
    by = c("site", "sex"),
    ci_method = "none",
    cluster = cluster
  )
  second <- calc_ltr_df(
    seer_example_data,
    by = c("site", "sex"),
    ci_method = "none",
    workers = 0,
    cluster = cluster
  )

  expect_identical(first, serial)
  expect_identical(second, serial)
  expect_error(calc_ltr_df(seer_example_data, cluster = 2), "cluster must")
})
