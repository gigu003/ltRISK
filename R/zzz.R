#' Calculate age widths from start age in each group
#'
#' @noRd
#' @param ages Start age in each group
#' @param last Width of the last age group
#'
#' @returns Width in each age group
#' @keywords internal
#'
calc_widths <- function(ages, last = Inf) {
  if (length(ages) < 1) {
    stop("ages vector must have at least one element")
  }

  # calculate widths in each age group
  if (length(ages) > 1) {
    widths <- diff(ages)
  } else {
    widths <- numeric(0)
  }

  # add width in last age group, default is Inf
  widths <- c(widths, last)
  return(widths)
}

validate_model_inputs <- function(
  ages,
  cancer,
  cancer_death,
  death,
  pys,
  type = c("developing", "dying"),
  last_age_widths = Inf
) {
  if (
    !is.character(type) ||
      length(type) != 1L ||
      !type %in% c("developing", "dying")
  ) {
    stop("type must be either 'developing' or 'dying'")
  }
  inputs <- list(
    ages = ages,
    cancer = cancer,
    cancer_death = cancer_death,
    death = death,
    pys = pys
  )
  lengths <- vapply(inputs, length, integer(1))
  if (length(unique(lengths)) != 1L || lengths[[1]] == 0L) {
    stop("All input vectors must have the same positive length")
  }
  if (any(!vapply(inputs, is.numeric, logical(1)))) {
    stop("All input vectors must be numeric")
  }
  if (any(!is.finite(unlist(inputs, use.names = FALSE)))) {
    stop("All input values must be finite")
  }
  if (length(ages) > 1L && any(diff(ages) <= 0)) {
    stop("ages must be strictly increasing with no duplicates")
  }
  if (any(cancer < 0) || any(cancer_death < 0) || any(death < 0)) {
    stop("Event counts must be non-negative")
  }
  if (any(pys <= 0)) {
    stop("pys must contain positive values")
  }
  if (any(cancer_death > death)) {
    stop("cancer_death must not exceed death")
  }
  if (
    !is.numeric(last_age_widths) ||
      length(last_age_widths) != 1L ||
      is.na(last_age_widths) ||
      last_age_widths <= 0
  ) {
    stop("last_age_widths must be a positive number or Inf")
  }
  type
}

validate_rate_inputs <- function(ages, rate_hat) {
  if (
    !is.numeric(ages) ||
      !is.numeric(rate_hat) ||
      length(ages) == 0L ||
      length(ages) != length(rate_hat)
  ) {
    stop(
      "ages and rate_hat must be numeric vectors of the same positive length"
    )
  }
  if (any(!is.finite(ages)) || any(!is.finite(rate_hat))) {
    stop("ages and rate_hat must contain only finite values")
  }
  if (length(ages) > 1L && any(diff(ages) <= 0)) {
    stop("ages must be strictly increasing with no duplicates")
  }
  if (any(rate_hat < 0)) {
    stop("rate_hat must be non-negative")
  }
  invisible(TRUE)
}

#' Generate possible combinations from start and end age
#'
#' @noRd
#' @param age_start Numeric vector of starting ages for each age group.
#' @param age_end Numeric vector of ending ages for each age group.
#' @param use_cross Logical indicating whether to generate cross combinations (default: TRUE).
#'
#' @return A data frame with columns \code{start} and \code{end} containing the
#' valid age combinations.
#' @keywords internal
#'
cross_age_range <- function(age_start, age_end, use_cross = TRUE) {
  if (use_cross) {
    combs <- expand.grid(start = age_start, end = age_end)
    combs <- combs[combs$start < combs$end, ]
    combs <- unique(combs) # 去除重复组合
    combs <- combs[order(combs$start, combs$end), ]
    row.names(combs) <- NULL
    return(combs)
  } else {
    n_start <- length(age_start)
    n_end <- length(age_end)
    if (n_start != n_end && n_start != 1L && n_end != 1L) {
      stop(
        "For pairwise age ranges, age_start and age_end must have equal lengths or one must have length 1"
      )
    }
    max_len <- max(n_start, n_end)
    combs <- data.frame(
      start = rep(age_start, length.out = max_len),
      end = rep(age_end, length.out = max_len)
    )
    if (any(combs$start >= combs$end)) {
      stop("Each pairwise age_end must be greater than its age_start")
    }
    row.names(combs) <- NULL
    return(combs)
  }
}

#' Match value position
#'
#' @noRd
#' @param x vector or NULL: the values to be matched.
#' @param table vector or NULL: the values to be matched against.
#'
#' @return A vector of the same length as x.
#' @keywords internal
#'
match2 <- function(x, table, nearest = FALSE) {
  if (nearest) {
    vapply(
      x,
      function(val) {
        which.min(abs(table - val))
      },
      integer(1)
    )
  } else {
    match(round(x, 6), round(table, 6))
  }
}
