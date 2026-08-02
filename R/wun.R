#' Compute probabilities used for estimation of lifetime risk
#'
#' @inheritParams amp
#' @param correct_for_surgery Logical value, whether correct for factors.
#' @param H Numeric vector of the number of surgeries (e.g., hysterectomies)
#'      in each interval. Required if \code{correct_for_surgery = TRUE}.
#'      Corresponds to H_x in the paper, representing the total number of
#'      hysterectomies performed in the interval.
#' @param Ch Numeric vector of the number of new cancer cases treated by the
#'      surgery in each interval. Required if \code{correct_for_surgery = TRUE}.
#'      Corresponds to Ch_x in the paper, representing the number of new
#'      cancer cases treated by a hysterectomy.
#' @param cohort_size Size of the cohort.
#'
#' @return A list of class "wun" containing:
#'   \itemize{
#'     \item \code{age}: The input ages.
#'     \item \code{contrib_a}: Contributions to new cancers (a_x).
#'     \item \code{condi_p}: Conditional probabilities.
#'     \item \code{l_cf}: Cancer-free (and surgery-free if applicable) alive at start.
#'     \item \code{d}: Non-cancer deaths among cancer-free.
#'     \item \code{s}: Non-cancer surgeries (if \code{correct_for_surgery = TRUE}).
#'   }
#'
#' @references
#'  Wun, L. M., Merrill, R. M., & Feuer, E. J. (1998). Estimating lifetime and
#'  age-conditional probabilities of developing cancer. Lifetime Data Analysis,
#'  4(2), 169-186.
#'
#'
#' @export
#'
wun <- function(
  ages,
  cancer,
  cancer_death,
  death,
  pys,
  correct_for_surgery = FALSE,
  H = NULL,
  Ch = NULL,
  type = "developing",
  cohort_size = 1,
  last_age_widths = Inf
) {
  type <- validate_model_inputs(
    ages,
    cancer,
    cancer_death,
    death,
    pys,
    type,
    last_age_widths
  )
  if (
    !is.logical(correct_for_surgery) ||
      length(correct_for_surgery) != 1L ||
      is.na(correct_for_surgery)
  ) {
    stop("correct_for_surgery must be TRUE or FALSE")
  }
  if (
    !is.numeric(cohort_size) ||
      length(cohort_size) != 1L ||
      !is.finite(cohort_size) ||
      cohort_size <= 0
  ) {
    stop("cohort_size must be a positive finite number")
  }
  if (correct_for_surgery) {
    if (is.null(H) || is.null(Ch)) {
      stop("H and Ch must be provided when correct_for_surgery is TRUE")
    }
    if (
      !is.numeric(H) ||
        !is.numeric(Ch) ||
        length(H) != length(ages) ||
        length(Ch) != length(ages) ||
        any(!is.finite(H)) ||
        any(!is.finite(Ch)) ||
        any(H < 0) ||
        any(Ch < 0)
    ) {
      stop("H and Ch must be non-negative finite numeric vectors matching ages")
    }
    if (any(Ch > H)) {
      stop("Ch must not exceed H")
    }
  }
  n_intervals <- length(ages)
  non_cancer_death <- death - cancer_death
  interval_width <- calc_widths(ages, last_age_widths)
  if (type == "dying") {
    cancer <- cancer_death
  }
  # Initialize vectors
  l <- rep(NA, n_intervals + 1) # Total alive at start
  l_cf <- rep(NA, n_intervals + 1) # Cancer-free (and surgery-free if applicable) alive at start (^0 l_x)
  a <- rep(NA, n_intervals) # New cancers (a_x)
  d <- rep(NA, n_intervals) # Non-cancer deaths among cancer-free (d_x)
  if (correct_for_surgery) {
    s <- rep(NA, n_intervals) # Non-cancer surgeries among cancer-free (s_x)
  }
  # Start cohort
  l[1] <- cohort_size
  l_cf[1] <- cohort_size
  # Loop over closed intervals (0-4 to 90-94)
  if (n_intervals > 1L) {
    for (i in seq_len(n_intervals - 1L)) {
      interval <- wun_closed_interval(
        i,
        interval_width[i],
        l[i],
        l_cf[i],
        cancer,
        non_cancer_death,
        death,
        pys,
        type,
        correct_for_surgery,
        H,
        Ch
      )
      a[i] <- interval$a
      d[i] <- interval$d
      if (correct_for_surgery) {
        s[i] <- interval$s
      }
      l[i + 1L] <- interval$l_next
      l_cf[i + 1L] <- interval$l_cf_next
    }
  }
  # Last interval
  i <- n_intervals
  r_x <- cancer[i] / pys[i]
  m_x <- death[i] / pys[i]
  if (is.finite(last_age_widths)) {
    interval <- wun_closed_interval(
      i,
      interval_width[i],
      l[i],
      l_cf[i],
      cancer,
      non_cancer_death,
      death,
      pys,
      type,
      correct_for_surgery,
      H,
      Ch
    )
    a[i] <- interval$a
    d[i] <- interval$d
    if (correct_for_surgery) {
      s[i] <- interval$s
    }
    l[i + 1L] <- interval$l_next
    l_cf[i + 1L] <- interval$l_cf_next
  } else {
    # Open-ended interval
    if (m_x == 0) {
      if (r_x == 0 && (!correct_for_surgery || H[i] == Ch[i])) {
        a[i] <- d[i] <- 0
        if (correct_for_surgery) s[i] <- 0
      } else {
        stop(
          "The open-ended age interval requires a positive all-cause mortality rate"
        )
      }
    } else {
      m0_x <- non_cancer_death[i] / pys[i]
      omega <- (l[i] / (m_x * l_cf[i])) * r_x
      epsilon <- 0
      if (correct_for_surgery) {
        h0_x <- (H[i] - Ch[i]) / pys[i]
        epsilon <- (l[i] / (m_x * l_cf[i])) * h0_x
      }
      if (omega + epsilon >= 1) {
        stop("Omega + epsilon >= 1; check data")
      }
      denominator <- 1 - omega - epsilon
      r_cf <- (omega / denominator) * m0_x
      h0_cf <- (epsilon / denominator) * m0_x
      combined_rate <- r_cf + m0_x + h0_cf
      if (combined_rate == 0) {
        a[i] <- d[i] <- 0
        if (correct_for_surgery) s[i] <- 0
      } else {
        a[i] <- l_cf[i] * (r_cf / combined_rate)
        d[i] <- l_cf[i] * (m0_x / combined_rate)
        if (correct_for_surgery) {
          s[i] <- l_cf[i] * (h0_cf / combined_rate)
        }
      }
    }
    # No l[i+1] or l_cf[i+1] for open interval
  }
  # Trim l_cf and l if open or closed
  l_cf <- head(l_cf, n_intervals)
  l <- head(l, n_intervals)
  # Compute age-conditional risks: sum of remaining a / l_cf at start
  remaining_a <- rev(cumsum(rev(a)))
  condi_prob <- remaining_a / l_cf
  res <- list(age = ages, contrib_a = a, condi_p = condi_prob, l_cf = l_cf)
  if (correct_for_surgery) {
    res$s <- s
    res$d <- d
  } else {
    res$d <- d
  }
  class(res) <- "wun"
  attr(res, "last_age_widths") <- last_age_widths
  return(res)
}

wun_closed_interval <- function(
  i,
  width,
  l,
  l_cf,
  cancer,
  non_cancer_death,
  death,
  pys,
  type,
  correct_for_surgery,
  H,
  Ch
) {
  r_x <- cancer[i] / pys[i]
  m_x <- death[i] / pys[i]
  m0_x <- non_cancer_death[i] / pys[i]
  prev_corr <- if (type == "dying") 1 else l / l_cf
  g_cf <- -expm1(-width * r_x) * prev_corr
  if (g_cf >= 1) {
    stop("Adjusted g_cf >= 1; check data or prevalence")
  }
  r_cf <- -log1p(-g_cf) / width
  h0_cf <- 0
  if (correct_for_surgery) {
    h0_x <- (H[i] - Ch[i]) / pys[i]
    f_cf <- -expm1(-width * h0_x) * prev_corr
    if (f_cf >= 1) {
      stop("Adjusted f_cf >= 1; check data or prevalence")
    }
    h0_cf <- -log1p(-f_cf) / width
  }

  combined_rate <- m0_x + r_cf + h0_cf
  if (combined_rate == 0) {
    exits <- 0
    allocation <- c(a = 0, d = 0, s = 0)
  } else {
    exits <- l_cf * -expm1(-width * combined_rate)
    allocation <- exits * c(r_cf, m0_x, h0_cf) / combined_rate
    names(allocation) <- c("a", "d", "s")
  }
  list(
    a = unname(allocation["a"]),
    d = unname(allocation["d"]),
    s = unname(allocation["s"]),
    l_next = l * exp(-width * m_x),
    l_cf_next = l_cf - exits
  )
}
