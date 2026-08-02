#' Compute probabilities used for estimation of lifetime risk
#'
#'
#' @param ages Numeric vector. Starting ages of each age
#' interval (e.g., 0, 5, 10, ..., 85).
#' @param cancer Numeric vector. Number of first cancer diagnoses in each age
#' interval.
#' @param death Numeric vector. Number of all deaths (all causes combined) in
#' each interval.
#' @param cancer_death Numeric vector. Number of deaths due to cancer in each
#' interval.
#' @param pys Numeric vector. Person-years at risk corresponding to each age
#' interval.
#' @param age_widths Numeric. Width of each age interval
#' (default = 5 years).
#' @param use_pmaj Logical. If TRUE, use PMAJ smoothing for rates
#' (default = FALSE).
#' @param pmaj_sub_interval Numeric. Sub-interval size for PMAJ
#' (default = 0.5 years).
#' @noRd
#' @keywords internal
compute_probs <- function(
  ages,
  cancer,
  death,
  cancer_death,
  pys,
  use_pmaj,
  pmaj_sub_interval = 0.5,
  age_widths = 5
) {
  n_intervals <- length(pys)
  orig_ages <- ages
  orig_deltas <- c(rep(age_widths, n_intervals - 1), Inf)
  non_cancer_death <- death - cancer_death
  if (use_pmaj) {
    rate_hat_c <- cancer / pys
    rate_hat_d <- cancer_death / pys
    rate_hat_o <- non_cancer_death / pys
    pmaj_c <- pmaj(orig_ages, rate_hat_c, pmaj_sub_interval)
    pmaj_d <- pmaj(orig_ages, rate_hat_d, pmaj_sub_interval)
    pmaj_o <- pmaj(orig_ages, rate_hat_o, pmaj_sub_interval)
    fine_l_c <- pmaj_c$fine_l
    fine_l_d <- pmaj_d$fine_l
    fine_l_o <- pmaj_o$fine_l
    fine_deltas <- pmaj_c$fine_deltas
    fine_start_indices <- pmaj_c$fine_start_indices
    n_intervals <- length(fine_deltas)
  } else {
    l_c <- cancer / pys
    l_d <- cancer_death / pys
    l_o <- non_cancer_death / pys
    deltas <- orig_deltas
    fine_start_indices <- seq_len(n_intervals)
    fine_l_c <- l_c
    fine_l_d <- l_d
    fine_l_o <- l_o
    fine_deltas <- deltas
  }
  s_d <- rep(NA, n_intervals + 1)
  s_o <- rep(NA, n_intervals + 1)
  s <- rep(NA, n_intervals + 1)
  cumul_prev <- rep(0, n_intervals + 1)
  s_c <- rep(NA, n_intervals + 1)
  contrib_a <- rep(NA, n_intervals)
  s_d[1] <- 1
  s_o[1] <- 1
  s[1] <- 1
  s_c[1] <- 1
  for (i in 1:n_intervals) {
    l_a <- fine_l_d[i] + fine_l_o[i]
    delta <- fine_deltas[i]
    if (is.finite(delta)) {
      if (fine_l_d[i] == 0) {
        contrib_d <- fine_l_c[i] * s_d[i] * delta
      } else {
        contrib_d <- fine_l_c[i] *
          s_d[i] /
          fine_l_d[i] *
          (1 - exp(-fine_l_d[i] * delta))
      }
    } else {
      if (fine_l_d[i] == 0) {
        contrib_d <- if (fine_l_c[i] == 0) 0 else Inf
      } else {
        contrib_d <- fine_l_c[i] * s_d[i] / fine_l_d[i]
      }
    }
    cumul_prev[i + 1] <- cumul_prev[i] + contrib_d
    s_c[i + 1] <- 1 - cumul_prev[i + 1]
    if (is.finite(delta)) {
      if (l_a == 0) {
        contrib_a[i] <- fine_l_c[i] * s[i] * delta
      } else {
        contrib_a[i] <- fine_l_c[i] * s[i] / l_a * (1 - exp(-l_a * delta))
      }
    } else {
      if (l_a == 0) {
        contrib_a[i] <- if (fine_l_c[i] == 0) 0 else Inf
      } else {
        contrib_a[i] <- fine_l_c[i] * s[i] / l_a
      }
    }
    if (is.finite(delta)) {
      s_d[i + 1] <- s_d[i] * exp(-fine_l_d[i] * delta)
      s_o[i + 1] <- s_o[i] * exp(-fine_l_o[i] * delta)
      s[i + 1] <- s_d[i + 1] * s_o[i + 1]
    } else {
      s_d[i + 1] <- 0
      s_o[i + 1] <- 0
      s[i + 1] <- 0
    }
  }
  orig_s_o <- s_o[fine_start_indices]
  orig_s_c <- s_c[fine_start_indices]
  orig_s <- s[fine_start_indices]
  orig_contrib_a <- numeric(length(orig_ages))
  n_orig <- length(fine_start_indices)
  for (i in 1:(n_orig - 1)) {
    start_idx <- fine_start_indices[i]
    end_idx <- fine_start_indices[i + 1] - 1
    orig_contrib_a[i] <- sum(contrib_a[start_idx:end_idx])
  }
  orig_contrib_a[n_orig] <- tail(contrib_a, 1)
  #remaining_contrib <- rev(cumsum(rev(contrib_a)))
  #orig_remaining <- remaining_contrib[fine_start_indices]
  #cond_prob <- orig_remaining / (orig_s_o * orig_s_c)
  if (any(orig_s_c > 1)) {
    warning(
      "Impossible cohort detected; check data for zero deaths in oldest groups or inconsistencies."
    )
  }
  list(
    orig_s_o = orig_s_o,
    orig_s_c = orig_s_c,
    orig_s = orig_s,
    orig_contrib_a = orig_contrib_a
  )
}

#' Compute gamma confidence interval for the probability estimate
#'
#' @param base_A Numeric. The base probability estimate.
#' @param alpha Numeric. The alpha level for the confidence interval (1 - conf.level).
#' @param cancer Numeric vector. Number of first cancer diagnoses.
#' @param death Numeric vector. Number of all deaths.
#' @param cancer_death Numeric vector. Number of deaths due to cancer.
#' @param pys Numeric vector. Person-years at risk.
#' @param idx_start Integer. Starting index for the interval.
#' @param idx_end Integer. Ending index for the interval.
#' @param ages Numeric vector. Starting ages.
#' @param age_widths Numeric. Interval width.
#' @param use_pmaj Logical. Use PMAJ smoothing.
#' @param pmaj_sub_interval Numeric. PMAJ sub-interval size.
#'
#' @return A named numeric vector with 'lower' and 'upper' bounds.
#' @importFrom stats qgamma
#' @noRd
#' @keywords internal
esti_ci <- function(
  base_A,
  alpha,
  cancer,
  death,
  cancer_death,
  pys,
  idx_start,
  idx_end,
  ages,
  age_widths,
  use_pmaj,
  pmaj_sub_interval
) {
  n <- length(ages)
  z_base <- c(cancer, cancer_death, death - cancer_death)
  # Compute delta_A
  delta_A <- numeric(3 * n)
  for (l in 1:(3 * n)) {
    new_cancer <- cancer
    new_cancer_death <- cancer_death
    new_death <- death
    inc <- 1
    if (l <= n) {
      new_cancer[l] <- cancer[l] + inc
    } else if (l <= 2 * n) {
      ll <- l - n
      new_cancer_death[ll] <- cancer_death[ll] + inc
      new_death[ll] <- death[ll] + inc
    } else {
      ll <- l - 2 * n
      new_non <- (death - cancer_death)[ll] + inc
      new_death[ll] <- cancer_death[ll] + new_non
    }
    new_res <- compute_probs(
      ages = ages,
      cancer = new_cancer,
      death = new_death,
      cancer_death = new_cancer_death,
      pys = pys,
      use_pmaj = use_pmaj,
      pmaj_sub_interval = pmaj_sub_interval,
      age_widths = age_widths
    )
    new_sum_contrib <- sum(new_res$orig_contrib_a[idx_start:idx_end])
    new_denom <- new_res$orig_s_o[idx_start] * new_res$orig_s_c[idx_start]
    new_A <- new_sum_contrib / new_denom
    delta_A[l] <- new_A - base_A
  }
  V <- sum(delta_A^2 * z_base)
  if (V == 0 || !is.finite(V)) {
    ci_lower <- base_A
  } else {
    gamma_shape <- base_A^2 / V
    gamma_scale <- V / base_A
    ci_lower <- qgamma(alpha / 2, shape = gamma_shape, scale = gamma_scale)
  }
  # Find max_A
  max_A <- base_A
  max_cancer <- cancer
  max_cancer_death <- cancer_death
  max_death <- death
  for (dir in c(1, -1)) {
    for (l in 1:(3 * n)) {
      if (dir == -1 && z_base[l] == 0) {
        next
      }
      inc <- dir
      new_cancer <- cancer
      new_cancer_death <- cancer_death
      new_death <- death
      if (l <= n) {
        new_cancer[l] <- cancer[l] + inc
        if (new_cancer[l] < 0) next
      } else if (l <= 2 * n) {
        ll <- l - n
        new_cancer_death[ll] <- cancer_death[ll] + inc
        if (new_cancer_death[ll] < 0) {
          next
        }
        new_death[ll] <- death[ll] + inc
      } else {
        ll <- l - 2 * n
        new_non <- (death - cancer_death)[ll] + inc
        if (new_non < 0) {
          next
        }
        new_death[ll] <- cancer_death[ll] + new_non
      }
      new_res <- compute_probs(
        ages = ages,
        cancer = new_cancer,
        death = new_death,
        cancer_death = new_cancer_death,
        pys = pys,
        use_pmaj = use_pmaj,
        pmaj_sub_interval = pmaj_sub_interval,
        age_widths = age_widths
      )
      new_sum_contrib <- sum(new_res$orig_contrib_a[idx_start:idx_end])
      new_denom <- new_res$orig_s_o[idx_start] * new_res$orig_s_c[idx_start]
      new_A <- new_sum_contrib / new_denom
      if (new_A > max_A) {
        max_A <- new_A
        max_cancer <- new_cancer
        max_cancer_death <- new_cancer_death
        max_death <- new_death
      }
    }
  }
  # Compute V at max
  delta_A_max <- numeric(3 * n)
  for (l in 1:(3 * n)) {
    new_cancer <- max_cancer
    new_cancer_death <- max_cancer_death
    new_death <- max_death
    inc <- 1
    if (l <= n) {
      new_cancer[l] <- max_cancer[l] + inc
    } else if (l <= 2 * n) {
      ll <- l - n
      new_cancer_death[ll] <- max_cancer_death[ll] + inc
      new_death[ll] <- max_death[ll] + inc
    } else {
      ll <- l - 2 * n
      new_non <- (max_death - max_cancer_death)[ll] + inc
      new_death[ll] <- new_cancer_death[ll] + new_non
    }
    new_res <- compute_probs(
      ages = ages,
      cancer = new_cancer,
      death = new_death,
      cancer_death = new_cancer_death,
      pys = pys,
      use_pmaj = use_pmaj,
      pmaj_sub_interval = pmaj_sub_interval,
      age_widths = age_widths
    )
    new_sum_contrib <- sum(new_res$orig_contrib_a[idx_start:idx_end])
    new_denom <- new_res$orig_s_o[idx_start] * new_res$orig_s_c[idx_start]
    new_A <- new_sum_contrib / new_denom
    delta_A_max[l] <- new_A - max_A
  }
  V_max <- sum(
    delta_A_max^2 *
      c(max_cancer, max_cancer_death, max_death - max_cancer_death)
  )
  if (V_max == 0 || !is.finite(V_max)) {
    ci_upper <- max_A
  } else {
    gamma_shape_max <- max_A^2 / V_max
    gamma_scale_max <- V_max / max_A
    ci_upper <- qgamma(
      1 - alpha / 2,
      shape = gamma_shape_max,
      scale = gamma_scale_max
    )
  }
  c(lower = ci_lower, upper = ci_upper)
}


#' Smooth Rates Using Piecewise Mid-Age Group Joinpoint (PMAJ) Method
#'
#' This function implements the Piecewise Mid-Age Group Joinpoint (PMAJ) smoothing method
#' to approximate smoothed rates for cancer incidence, cancer deaths, and other deaths
#' over finer sub-intervals. It uses linear interpolation between midpoints of original
#' age intervals to create piecewise constant rates on a finer grid.
#'
#' @param ages Numeric vector. Starting ages of the original age intervals (e.g., 0, 5, 10, ..., 95).
#' @param rate_hat_c Numeric vector. Estimated cancer incidence rates for the original intervals.
#' @param rate_hat_d Numeric vector. Estimated cancer death rates for the original intervals.
#' @param rate_hat_o Numeric vector. Estimated other cause death rates for the original intervals.
#' @param pmaj_sub_interval Numeric scalar. The width of sub-intervals for the PMAJ approximation (default = 0.5 years).
#'
#' @returns A list containing the following components:
#' \describe{
#'   \item{fine_l_c}{Numeric vector of smoothed cancer incidence rates on the fine grid.}
#'   \item{fine_l_d}{Numeric vector of smoothed cancer death rates on the fine grid.}
#'   \item{fine_l_o}{Numeric vector of smoothed other death rates on the fine grid.}
#'   \item{fine_deltas}{Numeric vector of sub-interval widths (mostly \code{pmaj_sub_interval}, with Inf for the last open interval).}
#'   \item{fine_start_indices}{Integer vector indicating the starting indices of the original intervals in the fine grid.}
#' }
#'
#' @export
#'
#' @examples
#' # Hypothetical small dataset
#' ages <- c(0, 5, 10, 15)
#' rate_hat_c <- c(0.001, 0.002, 0.003)
#' rate_hat_d <- c(0.0005, 0.001, 0.0015)
#' rate_hat_o <- c(0.01, 0.02, 0.03)
#' pmaj_smooth_rates(ages, rate_hat_c, rate_hat_d, rate_hat_o, pmaj_sub_interval = 1)
#'
#' # Using data from Fay et al. (2003) Table I for female breast cancer (simplified)
#' ages <- seq(0, 95, 5)
#' cancer <- c(0,0,1,9,43,335,1116,2670,5183,7392,8012,7341,7010,7651,8060,7146,4754,2574,952,273)
#' cancer_death <- c(0,0,1,0,6,35,173,425,765,1152,1427,1411,1436,1668,1920,1800,1533,1081,531,232)
#' death <- cancer_death + c(5893,561,627,1367,1541,2029,3012,4531,6234,8065,9976,12424,16957,25818,39434,51697,62624,63851,48324,26926)
#' pys <- c(4052953,4032790,3784789,3810986,3675646,4138795,4575728,4831799,4578168,3906260,3054146,2353577,1981443,1988371,1838556,1541002,1083867,629172,299128,114178)
#' rate_hat_c <- cancer / pys
#' rate_hat_d <- cancer_death / pys
#' rate_hat_o <- (death - cancer_death) / pys
#' pmaj_smooth_rates(ages, rate_hat_c, rate_hat_d, rate_hat_o)
#'
#' @references
#' Fay MP. Estimating age conditional probability of developing disease from surveillance data.
#' Popul Health Metr. 2004 Jul 27;2(1):6. doi: 10.1186/1478-7954-2-6. PMID: 15279675; PMCID: PMC517510.
#'
pmaj_smooth_rates <- function(
  ages,
  rate_hat_c,
  rate_hat_d,
  rate_hat_o,
  pmaj_sub_interval = 0.5
) {
  n_intervals <- length(ages)
  widths <- diff(ages)
  if (length(widths) == 0) {
    widths <- c(5)
  } # Default if single
  widths <- c(widths, tail(widths, 1)) # Nominal last width
  t_mid <- ages + widths / 2

  maj_rate <- function(t, t_mid, rate_mid) {
    if (t < t_mid[1]) {
      return(rate_mid[1])
    }
    if (t > tail(t_mid, 1)) {
      return(tail(rate_mid, 1))
    }
    i <- findInterval(t, t_mid)
    if (i == 0) {
      i <- 1
    }
    if (i == length(t_mid)) {
      return(rate_mid[i])
    }
    beta <- (rate_mid[i + 1] - rate_mid[i]) / (t_mid[i + 1] - t_mid[i])
    alpha <- rate_mid[i] - beta * t_mid[i]
    alpha + beta * t
  }

  fine_deltas <- c()
  fine_l_c <- c()
  fine_l_d <- c()
  fine_l_o <- c()
  fine_start_indices <- c(1)

  for (i in 1:(n_intervals - 1)) {
    sub_starts <- seq(ages[i], ages[i + 1], by = pmaj_sub_interval)
    mi <- length(sub_starts) - 1
    if (mi < 1) {
      stop("pmaj_sub_interval larger than interval width")
    }
    for (h in 1:mi) {
      sub_start <- sub_starts[h]
      sub_end <- sub_starts[h + 1]
      l_c_start <- maj_rate(sub_start, t_mid, rate_hat_c)
      l_c_end <- maj_rate(sub_end, t_mid, rate_hat_c)
      fine_l_c <- c(fine_l_c, (l_c_start + l_c_end) / 2)

      l_d_start <- maj_rate(sub_start, t_mid, rate_hat_d)
      l_d_end <- maj_rate(sub_end, t_mid, rate_hat_d)
      fine_l_d <- c(fine_l_d, (l_d_start + l_d_end) / 2)

      l_o_start <- maj_rate(sub_start, t_mid, rate_hat_o)
      l_o_end <- maj_rate(sub_end, t_mid, rate_hat_o)
      fine_l_o <- c(fine_l_o, (l_o_start + l_o_end) / 2)

      fine_deltas <- c(fine_deltas, pmaj_sub_interval)
    }
    fine_start_indices <- c(fine_start_indices, length(fine_deltas) + 1)
  }

  # Last infinite
  fine_l_c <- c(fine_l_c, maj_rate(Inf, t_mid, rate_hat_c))
  fine_l_d <- c(fine_l_d, maj_rate(Inf, t_mid, rate_hat_d))
  fine_l_o <- c(fine_l_o, maj_rate(Inf, t_mid, rate_hat_o))
  fine_deltas <- c(fine_deltas, Inf)

  list(
    fine_l_c = fine_l_c,
    fine_l_d = fine_l_d,
    fine_l_o = fine_l_o,
    fine_deltas = fine_deltas,
    fine_start_indices = fine_start_indices
  )
}


devcan2 <- function(
  ages,
  cancer,
  death,
  cancer_death,
  pys,
  age_widths = NULL,
  age_start = min(ages),
  age_end = Inf,
  pmaj_sub_interval = 0.05,
  maj_method = "pmaj"
) {
  if (is.null(age_widths)) {
    age_widths <- 5
  }
  n_intervals <- length(pys)
  orig_ages <- ages
  orig_deltas <- calc_widths(ages)
  #orig_deltas <- c(rep(age_widths, n_intervals - 1), Inf)
  non_cancer_death <- death - cancer_death
  if (maj_method == "pmaj") {
    rate_hat_c <- cancer / pys
    rate_hat_d <- cancer_death / pys
    rate_hat_o <- non_cancer_death / pys
    pmaj_c <- pmaj(
      orig_ages,
      rate_hat_c,
      pmaj_sub_interval,
      maj_method = maj_method
    )
    pmaj_d <- pmaj(
      orig_ages,
      rate_hat_d,
      pmaj_sub_interval,
      maj_method = maj_method
    )
    pmaj_o <- pmaj(
      orig_ages,
      rate_hat_o,
      pmaj_sub_interval,
      maj_method = maj_method
    )
    fine_l_c <- pmaj_c$fine_l
    fine_l_d <- pmaj_d$fine_l
    fine_l_o <- pmaj_o$fine_l
    fine_deltas <- pmaj_c$fine_deltas
    fine_start_indices <- pmaj_c$fine_start_indices
    n_intervals <- length(fine_deltas)
  } else {
    l_c <- cancer / pys
    l_d <- cancer_death / pys
    l_o <- non_cancer_death / pys
    deltas <- orig_deltas
    fine_start_indices <- seq_len(n_intervals)
    fine_l_c <- l_c
    fine_l_d <- l_d
    fine_l_o <- l_o
    fine_deltas <- deltas
  }
  s_d <- rep(NA, n_intervals + 1)
  s_o <- rep(NA, n_intervals + 1)
  s <- rep(NA, n_intervals + 1)
  cumul_prev <- rep(0, n_intervals + 1)
  s_c <- rep(NA, n_intervals + 1)
  contrib_a <- rep(NA, n_intervals)
  s_d[1] <- 1
  s_o[1] <- 1
  s[1] <- 1
  s_c[1] <- 1
  for (i in 1:n_intervals) {
    l_a <- fine_l_d[i] + fine_l_o[i]
    delta <- fine_deltas[i]
    if (is.finite(delta)) {
      if (fine_l_d[i] == 0) {
        contrib_d <- fine_l_c[i] * s_d[i] * delta
      } else {
        contrib_d <- fine_l_c[i] *
          s_d[i] /
          fine_l_d[i] *
          (1 - exp(-fine_l_d[i] * delta))
      }
    } else {
      if (fine_l_d[i] == 0) {
        contrib_d <- if (fine_l_c[i] == 0) 0 else Inf
      } else {
        contrib_d <- fine_l_c[i] * s_d[i] / fine_l_d[i]
      }
    }
    cumul_prev[i + 1] <- cumul_prev[i] + contrib_d
    s_c[i + 1] <- 1 - cumul_prev[i + 1]
    if (is.finite(delta)) {
      if (l_a == 0) {
        contrib_a[i] <- fine_l_c[i] * s[i] * delta
      } else {
        contrib_a[i] <- fine_l_c[i] * s[i] / l_a * (1 - exp(-l_a * delta))
      }
    } else {
      if (l_a == 0) {
        contrib_a[i] <- if (fine_l_c[i] == 0) 0 else Inf
      } else {
        contrib_a[i] <- fine_l_c[i] * s[i] / l_a
      }
    }
    if (is.finite(delta)) {
      s_d[i + 1] <- s_d[i] * exp(-fine_l_d[i] * delta)
      s_o[i + 1] <- s_o[i] * exp(-fine_l_o[i] * delta)
      s[i + 1] <- s_d[i + 1] * s_o[i + 1]
    } else {
      s_d[i + 1] <- 0
      s_o[i + 1] <- 0
      s[i + 1] <- 0
    }
  }
  orig_s_o <- s_o[fine_start_indices]
  orig_s_c <- s_c[fine_start_indices]
  orig_s <- s[fine_start_indices]
  orig_contrib_a <- numeric(length(orig_ages))
  n_orig <- length(fine_start_indices)
  for (i in 1:(n_orig - 1)) {
    start_idx <- fine_start_indices[i]
    end_idx <- fine_start_indices[i + 1] - 1
    orig_contrib_a[i] <- sum(contrib_a[start_idx:end_idx])
  }
  orig_contrib_a[n_orig] <- tail(contrib_a, 1)
  #remaining_contrib <- rev(cumsum(rev(contrib_a)))
  #orig_remaining <- remaining_contrib[fine_start_indices]
  #cond_prob <- orig_remaining / (orig_s_o * orig_s_c)
  if (any(orig_s_c > 1)) {
    warning(
      "Impossible cohort detected; check data for zero deaths in oldest groups or inconsistencies."
    )
  }

  idx_start <- match(age_start, ages)
  if (is.finite(age_end)) {
    idx_end <- match(age_end, ages) - 1
    if (idx_end < idx_start) {
      stop(paste("age_end must be greater than age_start"))
    }
  } else {
    idx_end <- length(orig_contrib_a)
  }
  sum_contrib <- sum(orig_contrib_a[idx_start:idx_end])
  denom <- orig_s_o[idx_start] * orig_s_c[idx_start]
  risk <- sum_contrib / denom

  params <- data.frame(
    ages = ages,
    contrib_a = orig_contrib_a,
    s = orig_s,
    s_o = orig_s_o,
    s_c = orig_s_c
  )
  list(risk = risk, params = params)
}


## 计算confidence Interval
#' Calculate probabilities and confidence interval
#'
#' @inheritParams amp
#' @param alpha Significance level for estimation of confidence intervals.
#' @param risk_func Function used to estimate cancer risk, options are "devcan"
#'      or "amp", the default is "devcan".
#' @param ci_method Method used to estimate confidence intervals, options are
#'      "gamma" or "delta", default is "gamma".
#' @param ... Additional arguments passed to the risk function.
#'
#' @returns list with risk, its lower bound and upper bound.
#' @export
#'
calc_risk_ci <- function(
  ages,
  cancer,
  cancer_death,
  death,
  pys,
  age_widths = NULL,
  age_start = 0,
  age_end = Inf,
  alpha = 0.05,
  risk_func = "devcan",
  ci_method = "gamma",
  ...
) {
  # Get the risk function object
  risk_func_obj <- get(risk_func)
  # Compute original risk using the specified risk function
  amp_out <- risk_func_obj(
    ages = ages,
    cancer = cancer,
    cancer_death = cancer_death,
    death = death,
    pys = pys,
    age_widths = age_widths,
    age_start = age_start,
    age_end = age_end,
    ...
  )
  risk <- amp_out$risk
  n <- length(ages)
  other_death <- death - cancer_death
  z <- c(cancer, cancer_death, other_death)
  # Function to compute risk with perturbed inputs using the specified risk function
  compute_risk <- function(pert_cancer, pert_cancer_death, pert_death) {
    risk_func_obj(
      ages = ages,
      cancer = pert_cancer,
      cancer_death = pert_cancer_death,
      death = pert_death,
      pys = pys,
      age_widths = age_widths,
      age_start = age_start,
      age_end = age_end,
      ...
    )$risk
  }
  # Compute delta_A for lower bound (perturb +1)
  delta_A <- numeric(3 * n)
  for (l in 1:(3 * n)) {
    if (l <= n) {
      pert_cancer <- cancer
      pert_cancer[l] <- cancer[l] + 1
      pert_cancer_death <- cancer_death
      pert_death <- death
    } else if (l <= 2 * n) {
      m <- l - n
      pert_cancer <- cancer
      pert_cancer_death <- cancer_death
      pert_cancer_death[m] <- cancer_death[m] + 1
      pert_death <- death
      pert_death[m] <- death[m] + 1
    } else {
      m <- l - 2 * n
      pert_cancer <- cancer
      pert_cancer_death <- cancer_death
      pert_death <- death
      pert_death[m] <- death[m] + 1
    }
    pert_risk <- compute_risk(pert_cancer, pert_cancer_death, pert_death)
    delta_A[l] <- pert_risk - risk
  }
  # Variance V
  z_var <- z
  if (ci_method == "delta") {
    z_var[z_var == 0] <- 0.5
  }
  V <- as.numeric(t(delta_A) %*% diag(z_var) %*% delta_A)
  if (V == 0) {
    V <- .Machine$double.eps
  } # Avoid division by zero
  if (ci_method == "gamma") {
    # Lower CI
    shape_L <- risk^2 / V
    scale_L <- V / risk
    L <- qgamma(alpha / 2, shape = shape_L, scale = scale_L)
    # Find z_M for upper bound
    max_A <- risk
    z_M <- z
    cancer_M <- cancer
    cancer_death_M <- cancer_death
    death_M <- death
    for (dir in c(1, -1)) {
      for (l in 1:(3 * n)) {
        if (l <= n) {
          pert_cancer <- cancer
          pert_cancer[l] <- max(0, cancer[l] + dir)
          pert_cancer_death <- cancer_death
          pert_death <- death
        } else if (l <= 2 * n) {
          m <- l - n
          pert_cancer <- cancer
          pert_cancer_death <- cancer_death
          pert_cancer_death[m] <- max(0, cancer_death[m] + dir)
          pert_death <- death
          pert_death[m] <- max(0, death[m] + dir)
        } else {
          m <- l - 2 * n
          pert_cancer <- cancer
          pert_cancer_death <- cancer_death
          pert_death <- death
          pert_death[m] <- max(0, death[m] + dir)
        }
        pert_risk <- compute_risk(pert_cancer, pert_cancer_death, pert_death)
        if (pert_risk > max_A) {
          max_A <- pert_risk
          cancer_M <- pert_cancer
          cancer_death_M <- pert_cancer_death
          death_M <- pert_death
          z_M <- c(
            pert_cancer,
            pert_cancer_death,
            pert_death - pert_cancer_death
          )
        }
      }
    }
    # Compute delta_A_M for z_M
    delta_A_M <- numeric(3 * n)
    for (l in 1:(3 * n)) {
      if (l <= n) {
        pert_cancer <- cancer_M
        pert_cancer[l] <- cancer_M[l] + 1
        pert_cancer_death <- cancer_death_M
        pert_death <- death_M
      } else if (l <= 2 * n) {
        m <- l - n
        pert_cancer <- cancer_M
        pert_cancer_death <- cancer_death_M
        pert_cancer_death[m] <- cancer_death_M[m] + 1
        pert_death <- death_M
        pert_death[m] <- death_M[m] + 1
      } else {
        m <- l - 2 * n
        pert_cancer <- cancer_M
        pert_cancer_death <- cancer_death_M
        pert_death <- death_M
        pert_death[m] <- death_M[m] + 1
      }
      pert_risk <- compute_risk(pert_cancer, pert_cancer_death, pert_death)
      delta_A_M[l] <- pert_risk - max_A
    }
    # Variance V_M
    V_M <- as.numeric(t(delta_A_M) %*% diag(z_M) %*% delta_A_M)
    if (V_M == 0) {
      V_M <- .Machine$double.eps
    }
    # Upper CI
    shape_M <- max_A^2 / V_M
    scale_M <- V_M / max_A
    U <- qgamma(1 - alpha / 2, shape = shape_M, scale = scale_M)
  } else if (ci_method == "delta") {
    z_val <- qnorm(1 - alpha / 2)
    sqrt_V <- sqrt(V)
    L <- risk - z_val * sqrt_V
    U <- risk + z_val * sqrt_V
  } else {
    stop('ci_method must be "gamma" or "delta"')
  }
  return(list(risk = risk, lower = L, upper = U))
}


find_max_risk2 <- function(cancer, cancer_death, death, base_risk) {
  n <- length(cancer)
  max_A <- base_risk
  other_death <- death - cancer_death
  z_M <- c(cancer, cancer_death, other_death)
  cancer_M <- cancer
  cancer_death_M <- cancer_death
  death_M <- death
  for (dir in c(1, -1)) {
    for (l in 1:(3 * n)) {
      if (l <= n) {
        pert_cancer <- cancer
        pert_cancer[l] <- max(0, cancer[l] + dir)
        pert_cancer_death <- cancer_death
        pert_death <- death
      } else if (l <= 2 * n) {
        m <- l - n
        pert_cancer <- cancer
        pert_cancer_death <- cancer_death
        pert_cancer_death[m] <- max(0, cancer_death[m] + dir)
        pert_death <- death
        pert_death[m] <- max(0, death[m] + dir)
      } else {
        m <- l - 2 * n
        pert_cancer <- cancer
        pert_cancer_death <- cancer_death
        pert_death <- death
        pert_death[m] <- max(0, death[m] + dir)
      }
      pert_risk <- calc_risk(pert_cancer, pert_cancer_death, pert_death)

      if (pert_risk > max_A) {
        max_A <- pert_risk
        cancer_M <- pert_cancer
        cancer_death_M <- pert_cancer_death
        death_M <- pert_death
      }
    }
  }
  return(
    list(
      risk = max_A,
      cancer_M = cancer_M,
      cancer_death_M = cancer_death_M,
      death_M = death_M
    )
  )
}


# Updated calc_delta (add r=NULL, pass to calc_risk)
calc_delta <- function(cancer, cancer_death, death, base_risk, r = NULL) {
  n <- length(cancer)
  # Determine nr based on whether r is provided
  nr <- if (is.null(r)) length(base_risk) else 1
  delta <- replicate(3 * n, numeric(nr), simplify = FALSE)
  for (l in 1:(3 * n)) {
    if (l <= n) {
      pert_cancer <- cancer
      pert_cancer[l] <- cancer[l] + 1
      pert_cancer_death <- cancer_death
      pert_death <- death
    } else if (l <= 2 * n) {
      m <- l - n
      pert_cancer <- cancer
      pert_cancer_death <- cancer_death
      pert_cancer_death[m] <- cancer_death[m] + 1
      pert_death <- death
      pert_death[m] <- death[m] + 1
    } else {
      m <- l - 2 * n
      pert_cancer <- cancer
      pert_cancer_death <- cancer_death
      pert_death <- death
      pert_death[m] <- death[m] + 1
    }
    pert_risk <- calc_risk(pert_cancer, pert_cancer_death, pert_death, r = r)
    delta[[l]] <- pert_risk - base_risk
  }
  delta <- lapply(1:nr, function(i) {
    sapply(delta, function(x) x[i])
  })
  return(delta)
}


find_max_risk <- function(cancer, cancer_death, death, base_risk) {
  n <- length(cancer)
  nrisks <- length(base_risk)
  max_A <- base_risk
  cancer_M <- vector("list", nrisks)
  cancer_death_M <- vector("list", nrisks)
  death_M <- vector("list", nrisks)
  for (r in 1:nrisks) {
    current_max <- base_risk[r]
    current_cancer_M <- cancer
    current_cancer_death_M <- cancer_death
    current_death_M <- death
    for (dir in c(1, -1)) {
      for (l in 1:(3 * n)) {
        if (l <= n) {
          pert_cancer <- cancer
          pert_cancer[l] <- max(0, cancer[l] + dir)
          pert_cancer_death <- cancer_death
          pert_death <- death
        } else if (l <= 2 * n) {
          m <- l - n
          pert_cancer <- cancer
          pert_cancer_death <- cancer_death
          pert_cancer_death[m] <- max(0, cancer_death[m] + dir)
          pert_death <- death
          pert_death[m] <- max(0, death[m] + dir)
        } else {
          m <- l - 2 * n
          pert_cancer <- cancer
          pert_cancer_death <- cancer_death
          pert_death <- death
          pert_death[m] <- max(0, death[m] + dir)
        }
        pert_risk <- calc_risk(pert_cancer, pert_cancer_death, pert_death)
        if (pert_risk[r] > current_max) {
          current_max <- pert_risk[r]
          current_cancer_M <- pert_cancer
          current_cancer_death_M <- pert_cancer_death
          current_death_M <- pert_death
        }
      }
    }
    max_A[r] <- current_max
    cancer_M[[r]] <- current_cancer_M
    cancer_death_M[[r]] <- current_cancer_death_M
    death_M[[r]] <- current_death_M
  }
  return(
    list(
      risk = max_A,
      cancer_M = cancer_M,
      cancer_death_M = cancer_death_M,
      death_M = death_M
    )
  )
}


amp2 <- function(
  ages,
  cancer,
  cancer_death,
  death,
  pys,
  age_widths = NULL,
  age_start = 0,
  age_end = 50
) {
  # Validate input lengths
  n <- length(cancer)
  #if (is.null(age_widths)) {
  #  age_widths <- rep(5, n)
  #  age_widths[n] <- Inf
  #} else if (length(age_widths) != n) {
  #  stop("age_widths must be of the same length as other inputs if provided")
  #}
  age_widths <- calc_widths(ages)
  if (
    length(cancer) != length(cancer_death) ||
      length(cancer) != length(death) ||
      length(cancer) != length(pys) ||
      length(cancer) != length(ages) ||
      length(cancer) != length(age_widths)
  ) {
    stop("All input vectors must be of the same length")
  }
  # Sort data by ages if not already sorted
  ord <- order(ages)
  ages <- ages[ord]
  age_widths <- age_widths[ord]
  cancer <- cancer[ord]
  cancer_death <- cancer_death[ord]
  death <- death[ord]
  pys <- pys[ord]
  # Check if age_start is one of the ages
  if (!(age_start %in% ages)) {
    stop("age_start must match one of the ages")
  }
  if (age_end < age_start) {
    stop("age_end must be greater than or equal to age_start")
  }
  # Find starting index
  start_idx <- which(ages == age_start)
  # Compute previous cumulative removal from age 0 to age_start
  prev_cum_removal <- 0
  if (start_idx > 1) {
    removal_rate_prev <- (cancer[1:(start_idx - 1)] +
      death[1:(start_idx - 1)] -
      cancer_death[1:(start_idx - 1)]) /
      pys[1:(start_idx - 1)]
    prev_cum_removal <- sum(removal_rate_prev * age_widths[1:(start_idx - 1)])
  }
  S0_cond <- exp(-prev_cum_removal)
  # Compute removal rates for the relevant bands
  removal_rate <- (cancer[start_idx:n] +
    death[start_idx:n] -
    cancer_death[start_idx:n]) /
    pys[start_idx:n]
  # Initialize integral and cumulative removal from age_start
  integral <- 0
  cum_removal_from_cond <- 0
  # Initialize vectors for details
  ages_out <- c()
  S0s <- c()
  contribs <- c()
  # Loop over bands from start_idx to n
  for (i in start_idx:n) {
    # If the band's start is beyond age_end, stop
    if (ages[i] >= age_end) {
      break
    }
    # S0_star at a_i
    S0_i <- S0_cond * exp(-cum_removal_from_cond)
    # Fraction: cancer_i / (cancer_i + death_i - cancer_death_i)
    denom <- cancer[i] + death[i] - cancer_death[i]
    frac <- ifelse(denom == 0, 0, cancer[i] / denom)
    # Effective width for this band, capped by age_end
    band_end <- ages[i] + age_widths[i]
    effective_end <- min(band_end, age_end)
    w_i <- effective_end - ages[i]
    # If effective width is zero or negative, skip
    if (w_i <= 0) {
      next
    }
    # Add to details
    ages_out <- c(ages_out, ages[i])
    S0s <- c(S0s, S0_i)
    # Removal rate for this band
    rem_rate_i <- removal_rate[i - start_idx + 1] # Adjust index for sliced vector
    # exp_term
    if (is.finite(w_i)) {
      exp_term <- 1 - exp(-w_i * rem_rate_i)
    } else {
      exp_term <- 1
    }
    # Contribution
    contrib <- frac * S0_i * exp_term
    integral <- integral + contrib
    # Add contribution to details
    contribs <- c(contribs, contrib)
    # Update cumulative removal (only if finite width)
    if (is.finite(w_i)) {
      cum_removal_from_cond <- cum_removal_from_cond + w_i * rem_rate_i
    }
    # If effective_end < band_end, we've reached the age_end, so break
    if (effective_end < band_end) break
  }
  # Conditional risk = integral / S0_cond
  cond_risk <- ifelse(S0_cond == 0, 0, integral / S0_cond)
  # Prepare details data frame
  details <- data.frame(
    S0ai = S0s,
    contrib_a = contribs
  )
  res <- list(
    risk = cond_risk,
    ages = ages_out,
    contrib_a = contribs,
    S0ai = S0s
  )
  class(res) <- "amp"
  return(res)
}


find_max_risk2 <- function(
  ages,
  cancer,
  cancer_death,
  death,
  pys,
  age_start = seq(0, 85, 5),
  age_end = 90,
  risk_func = "devcan",
  base_risk,
  ...
) {
  n <- length(cancer)
  max_A <- base_risk
  other_death <- death - cancer_death
  z_M <- c(cancer, cancer_death, other_death)
  cancer_M <- cancer
  cancer_death_M <- cancer_death
  death_M <- death
  for (dir in c(1, -1)) {
    for (l in 1:(3 * n)) {
      if (l <= n) {
        pert_cancer <- cancer
        pert_cancer[l] <- max(0, cancer[l] + dir)
        pert_cancer_death <- cancer_death
        pert_death <- death
      } else if (l <= 2 * n) {
        m <- l - n
        pert_cancer <- cancer
        pert_cancer_death <- cancer_death
        pert_cancer_death[m] <- max(0, cancer_death[m] + dir)
        pert_death <- death
        pert_death[m] <- max(0, death[m] + dir)
      } else {
        m <- l - 2 * n
        pert_cancer <- cancer
        pert_cancer_death <- cancer_death
        pert_death <- death
        pert_death[m] <- max(0, death[m] + dir)
      }

      pert_risk <- calc_risk(
        ages = ages,
        cancer = pert_cancer,
        cancer_death = pert_cancer_death,
        death = pert_death,
        pys = pys,
        age_start = age_start,
        age_end = age_end,
        risk_func = risk_func,
        ...
      )

      if (all(pert_risk > max_A)) {
        max_A <- pert_risk
        cancer_M <- pert_cancer
        cancer_death_M <- pert_cancer_death
        death_M <- pert_death
      }
    }
  }
  return(
    list(
      risk = max_A,
      cancer_M = cancer_M,
      cancer_death_M = cancer_death_M,
      death_M = death_M
    )
  )
}


#' Estimate lifetime and age-conditional probability of developing cancer
#'
#' This function estimates the lifetime and age-conditional probabilities of
#' developing cancer using a competing risks framework based on cross-sectional
#' incidence and mortality data. It computes probabilities for specified age
#' ranges, conditional on being alive and cancer-free at the start age, and
#' provides confidence intervals using either the gamma method (recommended
#' for small counts) or the delta method (normal approximation).
#'
#' @inheritParams devcan
#' @param age_start Initial age of lifetime risk.
#' @param age_end End age of lifetime risk.
#' @param type Characters "developing" or "dying" indicate estimate the
#'              probability of developing cancer or dying from it.
#' @param conf_level Confidence level, default is 0.95.
#' @param ci_method Method used to estimate confidence intervals, options are
#'     "gamma" or "delta".
#'
#' @returns A data frame with risk and confidence interval
#' @importFrom utils head
#' @importFrom stats qgamma
#' @references
#'
#' Fay MP, Pfeiffer R, Cronin KA, Le C, Feuer EJ. (2003). Age-conditional
#' probabilities of developing cancer. \emph{Statistics in Medicine},
#' 22(11):1837-1848. DOI: 10.1002/sim.1478.
#'
#' Fay M P. *Estimating age conditional probability of developing disease*
#' *from surveillance data\[J\]*. Population Health Metrics, 2004, 2(1): 6.
#'
#' @export
ltr_devcan <- function(
  ages = seq(0, 85, 5),
  cancer,
  death,
  cancer_death,
  pys,
  maj_method = "pmaj",
  pmaj_sub_interval = 0.5,
  age_start = min(ages),
  age_end = Inf,
  conf_level = 0.95,
  ci_method = "gamma",
  type = "developing"
) {
  range <- cross_age_range(age_start, age_end)
  res <- calc_risk_ci(
    ages = ages,
    cancer = cancer,
    cancer_death = cancer_death,
    death = death,
    pys = pys,
    age_start = age_start,
    age_end = age_end,
    alpha = 1 - conf_level,
    risk_func = "devcan",
    maj_method = maj_method,
    pmaj_sub_interval = pmaj_sub_interval,
    ci_method = ci_method,
    type = type
  )

  res_df <- data.frame(
    start = range$start,
    end = range$end,
    risk = res$risk,
    lower = res$lower,
    upper = res$upper
  )

  return(res_df)
}

#' Estimate the lifetime cancer risk adjusted for multiple primary cancers
#'
#' This function estimates the lifetime and age-conditional probabilities of
#' developing cancer, adjusted for multiple primary cancers, using a competing
#' risks framework based on the method described in Sasieni et al. (2011).
#' It computes probabilities for specified age ranges, conditional on being
#' alive and cancer-free at the start age, and provides confidence intervals
#' using either the gamma method (recommended for small counts) or the delta
#' method (normal approximation).
#'
#' @inheritParams amp
#' @param age_start Starting age(s) for the risk calculation (default = min(ages)).
#' @param age_end Ending age(s) for the risk calculation (default = Inf).
#'      Must match length of age_start.
#' @param type Characters "developing" or "dying" indicate estimate the
#'              probability of developing cancer or dying from it.
#' @param conf_level Confidence level, default is 0.95.
#' @param ci_method Method used to estimate confidence intervals, options are
#'     "gamma" or "delta".
#'
#' @returns Risk estimations and their confience intervals.
#' @export
#'
#' @references
#' Sasieni PD, Shelton J, Ormiston-Smith N, Thomson CS, Silcocks PB. What is
#' the lifetime risk of developing cancer?: the effect of adjusting for
#' multiple primaries. \emph{Br J Cancer}, 2011;105:460–465.
#' DOI: 10.1038/bjc.2011.250
#'
ltr_amp <- function(
  ages,
  cancer,
  cancer_death,
  death,
  pys,
  last_age_widths = Inf,
  age_start = 0,
  age_end = Inf,
  conf_level = 0.95,
  ci_method = "gamma",
  type = "dying"
) {
  if (type == "dying") {
    cancer <- cancer_death
  }
  range <- cross_age_range(age_start, age_end)
  risk <- calc_risk_ci(
    ages = ages,
    cancer = cancer,
    cancer_death = cancer_death,
    death = death,
    pys = pys,
    age_start = age_start,
    age_end = age_end,
    alpha = 1 - conf_level,
    risk_func = "amp",
    ci_method = ci_method
  )
  res <- data.frame(
    start = range$start,
    end = range$end,
    risk = risk$risk,
    lower = risk$lower,
    upper = risk$upper
  )

  return(res)
}


ltr_wun2 <- function(
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
  last_age_width = Inf
) {
  n_intervals <- length(ages)
  non_cancer_death <- death - cancer_death

  interval_width <- calc_widths(ages)
  if (type == "dying") {
    cancer <- cancer_death
  }

  # Initialize vectors
  l <- rep(NA, n_intervals + 1) # Total alive at start
  l_cf <- rep(NA, n_intervals + 1) # Cancer-free alive at start (^0 l_x)
  a <- rep(NA, n_intervals) # New cancers (a_x)
  d <- rep(NA, n_intervals) # Non-cancer deaths among cancer-free (d_x)
  if (correct_for_surgery) {
    s <- rep(NA, n_intervals) # Non-cancer surgeries among cancer-free (s_x)
  }
  # Start cohort
  l[1] <- cohort_size
  l_cf[1] <- cohort_size

  # Loop over closed intervals (0-4 to 90-94)
  for (i in 1:(n_intervals - 1)) {
    # Crude rates
    r_x <- cancer[i] / pys[i]
    g_x <- 1 - exp(-interval_width[i] * r_x)
    m_x <- death[i] / pys[i]
    q_x <- 1 - exp(-interval_width[i] * m_x)
    m0_x <- non_cancer_death[i] / pys[i]

    # Prevalence correction factor (l_x / ^0 l_x)
    prev_corr <- l[i] / l_cf[i]

    if (type == "dying") {
      prev_corr <- 1
    }

    # Adjusted probability and rate among cancer-free
    g_cf <- g_x * prev_corr
    if (g_cf >= 1) {
      stop("Adjusted g_cf >=1; check data or prevalence")
    }
    r_cf <- -(1 / interval_width[i]) * log(1 - g_cf)

    # Assume ^0 m0_x = m0_x
    if (correct_for_surgery) {
      # Surgery rates
      h_x <- H[i] / pys[i]
      hc_x <- Ch[i] / pys[i]
      h0_x <- h_x - hc_x # Non-cancer surgery rate

      # Assume ^0 h0_x = h0_x (similar to death rate assumption)
      # Combined rate
      combined_rate <- m0_x + r_cf + h0_x

      # Cancer-free (and surgery-free) at next interval
      l_cf_next <- l_cf[i] * exp(-interval_width[i] * combined_rate)

      # New cancers
      a[i] <- l_cf[i] *
        (1 - exp(-interval_width[i] * combined_rate)) *
        (r_cf / combined_rate)

      # Non-cancer deaths among cancer-free
      d[i] <- l_cf[i] *
        (1 - exp(-interval_width[i] * combined_rate)) *
        (m0_x / combined_rate)

      # Non-cancer surgeries among cancer-free
      s[i] <- l_cf[i] *
        (1 - exp(-interval_width[i] * combined_rate)) *
        (h0_x / combined_rate)
    } else {
      # Combined rate without surgery
      combined_rate <- m0_x + r_cf

      # Cancer-free at next interval
      l_cf_next <- l_cf[i] * exp(-interval_width[i] * combined_rate)

      # New cancers
      a[i] <- l_cf[i] *
        (1 - exp(-interval_width[i] * combined_rate)) *
        (r_cf / combined_rate)

      # Non-cancer deaths among cancer-free
      d[i] <- l_cf[i] *
        (1 - exp(-interval_width[i] * combined_rate)) *
        (m0_x / combined_rate)
    }
    # Total alive at next (from all-cause mortality)
    l[i + 1] <- l[i] * (1 - q_x)

    # Update cancer-free
    l_cf[i + 1] <- l_cf_next
  }

  # Open-ended interval (95+, last age groups, which is open)
  i <- n_intervals
  r_x <- cancer[i] / pys[i]
  m_x <- death[i] / pys[i]
  m0_x <- non_cancer_death[i] / pys[i]

  # For open interval, compute omega and adjusted r_cf
  omega <- (l[i] / (m_x * l_cf[i])) * r_x
  if (omega >= 1) {
    stop("Omega >=1; check data")
  }
  r_cf <- (omega / (1 - omega)) * m0_x

  if (correct_for_surgery) {
    # Surgery rates for open interval
    h_x <- H[i] / pys[i]
    hc_x <- Ch[i] / pys[i]
    h0_x <- h_x - hc_x

    # Combined rate
    combined_rate <- r_cf + m0_x + h0_x

    # New cancers
    a[i] <- l_cf[i] * (r_cf / combined_rate)

    # Non-cancer deaths
    d[i] <- l_cf[i] * (m0_x / combined_rate)

    # Non-cancer surgeries
    s[i] <- l_cf[i] * (h0_x / combined_rate)
  } else {
    # Combined rate without surgery
    combined_rate <- r_cf + m0_x

    # New cancers
    a[i] <- l_cf[i] * (r_cf / combined_rate)

    # Non-cancer deaths
    d[i] <- l_cf[i] * (m0_x / combined_rate)
  }

  l_cf <- head(l_cf, -1)
  # Compute age-conditional risks: sum of remaining a / l_cf at start
  remaining_a <- rev(cumsum(rev(a)))
  condi_prob <- remaining_a / l_cf
  return(list(age = ages, contrib_a = a, condi_p = condi_prob))
}
