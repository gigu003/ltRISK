#' Calculate the risk value for a specific age range
#'
#' @rdname get_risk
#' @param object Returned object from a risk model calculated using the devcan,
#'      amp, or wun function.
#' @param age_start Starting age(s) used to calculate risk over one or
#'      multiple age ranges.
#' @param age_end Ending age(s) used to calculate risk over one or
#'      multiple age ranges.
#' @param age_combine How age vectors are combined: `"cross"` or `"pairwise"`.
#' @return Risks according to the age ranges.
#' @export
#'
get_risk <- function(
  object,
  age_start,
  age_end,
  age_combine = c("cross", "pairwise")
) {
  age_combine <- match.arg(age_combine)
  UseMethod("get_risk", object)
}

#' @rdname get_risk
#' @method get_risk devcan
#' @export
#'
get_risk.devcan <- function(
  object,
  age_start,
  age_end,
  age_combine = c("cross", "pairwise")
) {
  age_combine <- match.arg(age_combine)
  range <- cross_age_range(
    age_start,
    age_end,
    use_cross = age_combine == "cross"
  )
  age_start <- range$start
  age_end <- range$end
  ages <- round(object$ages, 1)
  s_o <- object$s_o
  s_c <- object$s_c
  contrib_a <- object$contrib_a
  cum_contrib <- c(0, cumsum(contrib_a))
  idx_starts <- match(round(age_start, 6), round(ages, 6))
  if (any(is.na(idx_starts))) {
    stop(
      "Some age_start values are not available model age boundaries; use maj_method = \"pmaj\" for finer endpoints"
    )
  }
  idx_ends <- ifelse(
    is.finite(age_end),
    match(round(age_end, 6), round(ages, 6)) - 1,
    length(contrib_a)
  )
  if (any(is.na(idx_ends[is.finite(age_end)]))) {
    stop(
      "Some finite age_end values are not available model age boundaries; use maj_method = \"pmaj\" for finer endpoints"
    )
  }
  if (any(idx_ends < idx_starts)) {
    stop("age_end must be greater than age_start")
  }
  sum_contribs <- cum_contrib[idx_ends + 1] - cum_contrib[idx_starts]
  denoms <- s_o[idx_starts] * s_c[idx_starts]
  risks <- sum_contribs / denoms
  return(risks)
}


#' @rdname get_risk
#' @method get_risk amp
#' @export
#'
get_risk.amp <- function(
  object,
  age_start,
  age_end,
  age_combine = c("cross", "pairwise")
) {
  if (!inherits(object, "amp")) {
    stop("object must be of class 'amp'")
  }

  # expand for all possible age combination
  age_combine <- match.arg(age_combine)
  range <- cross_age_range(
    age_start,
    age_end,
    use_cross = age_combine == "cross"
  )
  age_start <- range$start
  age_end <- range$end

  # Extract data
  ages <- object$ages
  S0ai <- object$S0ai
  fracs <- object$fracs
  rem_rates <- object$rem_rates
  widths <- object$widths
  terminal_age <- if (is.finite(tail(widths, 1))) {
    tail(ages, 1) + tail(widths, 1)
  } else {
    Inf
  }
  idx_starts <- match2(age_start, ages)
  if (any(is.na(idx_starts))) {
    stop("Some age_start values are not available model age boundaries")
  }
  is_terminal <- is.finite(terminal_age) &
    round(age_end, 6) == round(terminal_age, 6)
  idx_ends <- ifelse(
    is.finite(age_end) & !is_terminal,
    match2(age_end, ages) - 1L,
    length(ages)
  )
  if (any(is.na(idx_ends[is.finite(age_end)]))) {
    stop("Some finite age_end values are not available model age boundaries")
  }
  if (any(idx_ends < idx_starts)) {
    stop("age_end must be greater than age_start")
  }

  full_contrib <- fracs *
    S0ai *
    ifelse(
      is.finite(widths),
      -expm1(-widths * rem_rates),
      1
    )
  cumulative <- c(0, cumsum(full_contrib))
  numerator <- cumulative[idx_ends + 1L] - cumulative[idx_starts]
  ifelse(S0ai[idx_starts] == 0, 0, numerator / S0ai[idx_starts])
}

#' @rdname get_risk
#' @method get_risk wun
#' @export
#'
get_risk.wun <- function(
  object,
  age_start,
  age_end,
  age_combine = c("cross", "pairwise")
) {
  age_combine <- match.arg(age_combine)
  range <- cross_age_range(
    age_start,
    age_end,
    use_cross = age_combine == "cross"
  )
  age_start <- range$start
  age_end <- range$end

  ages <- object$age
  contrib_a <- object$contrib_a
  cum_contrib <- c(0, cumsum(contrib_a))
  idx_starts <- match2(age_start, ages)
  if (any(is.na(idx_starts))) {
    stop("Some age_start not found in ages")
  }
  terminal_age <- tail(ages, 1) + attr(object, "last_age_widths", exact = TRUE)
  terminal <- is.finite(age_end) &
    is.finite(terminal_age) &
    round(age_end, 6) == round(terminal_age, 6)
  idx_ends <- ifelse(
    is.finite(age_end) & !terminal,
    match2(age_end, ages) - 1,
    length(contrib_a)
  )
  if (any(is.na(idx_ends[is.finite(age_end)]))) {
    stop("Some age_end not found in ages")
  }
  if (any(idx_ends < idx_starts)) {
    stop("age_end must be greater than age_start")
  }
  sum_contribs <- cum_contrib[idx_ends + 1] - cum_contrib[idx_starts]
  denoms <- object$l_cf[idx_starts]
  risks <- sum_contribs / denoms
  return(risks)
}


#' @rdname get_risk
#' @method get_risk cumu
#' @export
#'
get_risk.cumu <- function(
  object,
  age_start,
  age_end,
  age_combine = c("cross", "pairwise")
) {
  # 解析交叉年龄区间（如果你允许不同输入形式）
  age_combine <- match.arg(age_combine)
  range <- cross_age_range(
    age_start,
    age_end,
    use_cross = age_combine == "cross"
  )
  age_start <- range$start
  age_end <- range$end

  ages <- object$ages
  rate <- object$rate
  widths <- object$widths
  terminal_age <- if (is.finite(tail(widths, 1))) {
    tail(ages, 1) + tail(widths, 1)
  } else {
    Inf
  }
  rates <- widths * rate
  rates[widths == Inf & rate == 0] <- 0
  cum_contrib <- c(0, cumsum(rates))
  idx_starts <- match2(age_start, ages)
  if (any(is.na(idx_starts))) {
    stop("Some age_start not found in ages")
  }
  is_terminal <- is.finite(terminal_age) &
    round(age_end, 6) == round(terminal_age, 6)
  needs_match <- is.finite(age_end) & !is_terminal
  idx_ends <- rep.int(length(rates), length(age_end))
  idx_ends[needs_match] <- match2(age_end[needs_match], ages)
  if (any(is.na(idx_ends[needs_match]))) {
    stop("Some age_end not found in ages")
  }
  if (any(idx_ends < idx_starts)) {
    stop("age_end must be greater than age_start")
  }
  sum_contribs <- cum_contrib[idx_ends + 1] - cum_contrib[idx_starts]
  risks <- 1 - exp(-sum_contribs)
  return(risks)
}
