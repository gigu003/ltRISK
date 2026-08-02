#' Manage ltRISK in-memory caches
#'
#' `clear_ltr_cache()` removes memoised risk-model results and PMAJ grid
#' geometries from the current R process. It can also clear the corresponding
#' caches on an existing PSOCK cluster. `ltr_cache_info()` reports the number of
#' cached entries in the current process.
#'
#' @param cluster Optional cluster created by [parallel::makePSOCKcluster()].
#'   When supplied, caches are also cleared on every worker. The cluster remains
#'   running.
#'
#' @return `clear_ltr_cache()` invisibly returns `NULL`. `ltr_cache_info()`
#'   returns a data frame with cache names and entry counts.
#' @export
clear_ltr_cache <- function(cluster = NULL) {
  cluster <- ltr_validate_cluster(cluster)
  ltr_forget_caches()
  if (!is.null(cluster)) {
    parallel::clusterCall(cluster, ltr_forget_caches)
  }
  invisible(NULL)
}

#' @rdname clear_ltr_cache
#' @export
ltr_cache_info <- function() {
  data.frame(
    cache = c("risk", "pmaj_geometry"),
    entries = c(ltr_cache_entries(calc_risk), ltr_cache_entries(pmaj_geometry)),
    row.names = NULL
  )
}

ltr_forget_caches <- function() {
  memoise::forget(calc_risk)
  memoise::forget(pmaj_geometry)
  invisible(NULL)
}

ltr_cache_entries <- function(fun) {
  cache <- get("_cache", envir = environment(fun), inherits = FALSE)
  length(cache$keys())
}
