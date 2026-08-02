test_that("ltRISK caches can be inspected and cleared", {
  data("seer_example_data")
  clear_ltr_cache()
  expect_identical(ltr_cache_info()$entries, c(0L, 0L))

  breast <- seer_example_data[seer_example_data$site == "Breast", ]
  calc_ltr_df(breast, ci_method = "none")
  expect_gt(ltr_cache_info()$entries[[1]], 0)
  expect_gt(ltr_cache_info()$entries[[2]], 0)

  clear_ltr_cache()
  expect_identical(ltr_cache_info()$entries, c(0L, 0L))
})

test_that("grouped calculations can clear caches automatically", {
  data("seer_example_data")
  clear_ltr_cache()
  calc_ltr_df(
    seer_example_data,
    by = c("site", "sex"),
    ci_method = "none",
    cache = "clear"
  )
  expect_identical(ltr_cache_info()$entries, c(0L, 0L))
})

test_that("none cache policy discards entries after every grouped job", {
  data("seer_example_data")
  clear_ltr_cache()
  out <- calc_ltr_df(
    seer_example_data,
    by = c("site", "sex"),
    ci_method = "none",
    cache = "none"
  )
  expect_s3_class(out, "data.frame")
  expect_identical(ltr_cache_info()$entries, c(0L, 0L))

  expect_error(
    calc_ltr_df(seer_example_data, cache = "invalid"),
    "arg"
  )
})

test_that("external worker caches can be cleared without stopping the cluster", {
  skip_on_cran()
  data("seer_example_data")
  cluster <- parallel::makePSOCKcluster(2)
  on.exit(parallel::stopCluster(cluster), add = TRUE)

  out <- calc_ltr_df(
    seer_example_data,
    by = c("site", "sex"),
    ci_method = "none",
    cluster = cluster,
    cache = "clear"
  )
  expect_s3_class(out, "data.frame")
  expect_length(parallel::clusterCall(cluster, identity, TRUE), 2)
})

test_that("none cache policy leaves reusable worker caches empty", {
  skip_on_cran()
  data("seer_example_data")
  cluster <- parallel::makePSOCKcluster(2)
  on.exit(parallel::stopCluster(cluster), add = TRUE)

  calc_ltr_df(
    seer_example_data,
    by = c("site", "sex"),
    ci_method = "none",
    cluster = cluster,
    cache = "none"
  )
  entries <- parallel::clusterCall(cluster, ltr_cache_info)
  expect_true(all(vapply(
    entries,
    function(x) all(x$entries == 0L),
    logical(1)
  )))
})
