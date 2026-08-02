#' Estimate disease risk and its confidence interval
#'
#' Estimate disease risk using population-based cancer registry data,
#' including age-specific cancer incidence, cancer mortality, all-cause
#' mortality, and corresponding population data. Available methods include
#' adjusting for multiple primary cancers (AMP),
#' the DevCan method recommended by Surveillance, Epidemiology,
#' and End Results (SEER), and the Wun method.
#'
#' @inheritParams amp
#' @param age_start Starting age(s) used to calculate risk over one or
#'      multiple age ranges.
#' @param age_end Ending age(s) used to calculate risk over one or
#'      multiple age ranges.
#' @param age_combine How `age_start` and `age_end` are combined. `"cross"`
#'      (default) returns every valid combination. `"pairwise"` pairs elements
#'      positionally, allowing scalar recycling.
#' @param alpha Alpha level specifying the confidence level for estimating
#'      the risk confidence interval.
#' @param risk_func Function used to estimate cancer risk. Options are
#'     "devcan", "amp", "wun" or "cumulative", with "devcan" as the default.
#'     The legacy alias "cumu" is also accepted.
#' @param ci_method Method used to estimate confidence intervals for the risk.
#'      Options are "gamma", "delta", or "none", with "gamma" as the default.
#'      Use "none" to skip variance and confidence-interval calculations.
#' @param variance_method Method used to estimate the risk variance. "auto"
#'      uses an analytic gradient for DevCan, AMP, Wun, and cumulative-risk
#'      models and otherwise falls back to "finite_difference". Analytic
#'      gradients support constant and PMAJ rates where applicable. Wun models
#'      with surgery correction use finite differences. "analytic" requires an
#'      implemented analytic gradient. Default is "auto".
#' @param multiplier Numeric value to scale the risk and confidence interval.
#'        Default is 100, which reports risk as a percentage.
#' @param digits Integer indicating the number of decimal places to round the results.
#'        Default is 6.
#' @param return_variance Logical. If \code{TRUE}, also return the estimated
#'        variance and standard error of the risk on the same scale as
#'        \code{multiplier}. These values are not rounded by \code{digits},
#'        so they can be used for downstream statistical tests. Default is
#'        \code{FALSE}.
#' @param ... Additional arguments passed to the risk estimation function,
#'      depending on the value of risk_func. If risk_func = "devcan",
#'      see ?devcan for available parameters.
#'
#' Input counts must be non-negative, `pys` must be positive, and cancer deaths
#' must not exceed all-cause deaths. Variances treat cancer incidence, cancer
#' deaths, and other deaths as independent Poisson count components and treat
#' person-years as fixed. Gamma intervals return zero limits for an age range
#' with zero estimated risk and no positive finite local perturbation.
#'
#' @returns
#' A data frame containing the following variables:
#' \itemize{
#'   \item \code{start} — starting age of the age range for the risk.
#'   \item \code{end} — ending age of the age range for the risk.
#'   \item \code{risk} — estimated risk value.
#'   \item \code{lower} — lower bound of the confidence interval.
#'   \item \code{upper} — upper bound of the confidence interval.
#'   \item \code{variance} — estimated variance of the risk, returned only
#'   when \code{return_variance = TRUE}.
#'   \item \code{se} — estimated standard error of the risk, returned only
#'   when \code{return_variance = TRUE}.
#' }
#' @export
#'
#' @references
#' Sasieni PD, Shelton J, Ormiston-Smith N, Thomson CS, Silcocks PB. What is
#' the lifetime risk of developing cancer?: the effect of adjusting for
#' multiple primaries. \emph{Br J Cancer}, 2011;105:460–465.
#' DOI: 10.1038/bjc.2011.250
#'
#' Fay MP, Pfeiffer R, Cronin KA, Le C, Feuer EJ. (2003). *Age-conditional*
#' *probabilities of developing cancer*. \emph{Statistics in Medicine},
#' 22(11):1837-1848. DOI: 10.1002/sim.1428.
#'
#' Fay M P. *Estimating age conditional probability of developing disease*
#' *from surveillance data\[J\]*. Population Health Metrics, 2004, 2(1): 6.
#'
#' @examples
#' breast <- seer_example_data[seer_example_data$site=="Breast",]
#' calc_ltr(ages = breast$ages, cancer = breast$cancer,
#'          cancer_death = breast$cancer_death, death = breast$death,
#'          pys = breast$pys, maj_method = "constant",
#'          age_start = c(0, 30, 50, 70), age_end = c(30, 50, 70, Inf),
#'          digits = 4)
#'
calc_ltr <- function(
  ages,
  cancer,
  cancer_death,
  death,
  pys,
  age_start = min(ages),
  age_end = Inf,
  age_combine = c("cross", "pairwise"),
  alpha = 0.05,
  risk_func = "devcan",
  ci_method = "gamma",
  variance_method = "auto",
  multiplier = 100,
  digits = 6,
  return_variance = FALSE,
  ...
) {
  age_combine <- match.arg(age_combine)
  validate_ltr_inputs(
    ages,
    cancer,
    cancer_death,
    death,
    pys,
    alpha,
    age_start,
    age_end,
    multiplier,
    digits,
    return_variance
  )
  ci_method <- match.arg(ci_method, c("gamma", "delta", "none"))
  variance_method <- match.arg(
    variance_method,
    c("auto", "analytic", "finite_difference")
  )
  if (ci_method == "none" && return_variance) {
    stop("return_variance cannot be TRUE when ci_method = \"none\"")
  }
  res <- calc_risk(
    ages = ages,
    cancer = cancer,
    cancer_death = cancer_death,
    death = death,
    pys = pys,
    risk_func = risk_func,
    ...
  )
  risk <- get_risk(res, age_start, age_end, age_combine = age_combine)

  range <- cross_age_range(
    age_start,
    age_end,
    use_cross = age_combine == "cross"
  )
  if (ci_method == "none") {
    out <- data.frame(
      start = range$start,
      end = range$end,
      risk = round(risk * multiplier, digits)
    )
    attr(out, "risk_digits") <- as.integer(digits)
    return(out)
  }

  V <- calc_variance(
    ages = ages,
    cancer = cancer,
    cancer_death = cancer_death,
    death = death,
    pys = pys,
    age_start = age_start,
    age_end = age_end,
    age_combine = age_combine,
    risk_func = risk_func,
    base_risk = risk,
    ci_method = ci_method,
    variance_method = variance_method,
    ...
  )
  if (any(!is.finite(V)) || any(V < 0)) {
    stop(
      "Risk variance is non-finite or negative; check the oldest age interval and event counts"
    )
  }

  risk_length <- length(range$start)

  if (ci_method == "gamma") {
    # Lower CI
    L <- risk
    variable_risk <- risk > 0 & V > 0
    shape_L <- risk[variable_risk]^2 / V[variable_risk]
    scale_L <- V[variable_risk] / risk[variable_risk]
    L[variable_risk] <- qgamma(
      alpha / 2,
      shape = shape_L,
      scale = scale_L
    )

    res <- find_max_risk(
      ages = ages,
      cancer = cancer,
      cancer_death = cancer_death,
      death = death,
      pys = pys,
      age_start = age_start,
      age_end = age_end,
      age_combine = age_combine,
      risk_func = risk_func,
      base_risk = risk,
      ...
    )

    cancer_M <- res$cancer_M
    cancer_death_M <- res$cancer_death_M
    death_M <- res$death_M
    max_A <- res$risk
    max_keys <- vapply(
      seq_len(risk_length),
      function(i) {
        paste(
          c(cancer_M[[i]], cancer_death_M[[i]], death_M[[i]]),
          collapse = ":"
        )
      },
      character(1)
    )
    V_M <- numeric(risk_length)
    for (key in unique(max_keys)) {
      idx <- which(max_keys == key)
      first <- idx[[1]]
      V_M[idx] <- calc_variance(
        ages = ages,
        cancer = cancer_M[[first]],
        cancer_death = cancer_death_M[[first]],
        death = death_M[[first]],
        pys = pys,
        age_start = range$start[idx],
        age_end = range$end[idx],
        age_combine = "pairwise",
        risk_func = risk_func,
        base_risk = max_A[idx],
        ci_method = ci_method,
        variance_method = variance_method,
        ...
      )
    }
    # Upper CI
    U <- max_A
    variable_max <- max_A > 0 & V_M > 0
    shape_M <- max_A[variable_max]^2 / V_M[variable_max]
    scale_M <- V_M[variable_max] / max_A[variable_max]
    U[variable_max] <- qgamma(
      1 - alpha / 2,
      shape = shape_M,
      scale = scale_M
    )
  } else if (ci_method == "delta") {
    z_val <- qnorm(1 - alpha / 2)
    sqrt_V <- sqrt(V)
    L <- risk - z_val * sqrt_V
    U <- risk + z_val * sqrt_V
  }

  out <- data.frame(
    start = range$start,
    end = range$end,
    risk = round(risk * multiplier, digits),
    lower = round(L * multiplier, digits),
    upper = round(U * multiplier, digits)
  )

  if (return_variance) {
    out$variance <- V * multiplier^2
    out$se <- sqrt(V) * multiplier
  }

  attr(out, "risk_digits") <- as.integer(digits)
  out
}


# Internal memoised model constructor.
calc_risk <- memoise::memoise(
  function(ages, cancer, cancer_death, death, pys, risk_func = "devcan", ...) {
    if (!risk_func %in% c("devcan", "amp", "wun", "cumulative", "cumu")) {
      stop('risk_func must be "devcan", "amp", "wun", "cumulative" or "cumu"')
    }

    func_name <- if (risk_func == "cumu") "cumulative" else risk_func
    result_class <- if (risk_func %in% c("cumulative", "cumu")) {
      "cumu"
    } else {
      risk_func
    }

    func <- get(func_name, mode = "function")
    result <- func(
      ages = ages,
      cancer = cancer,
      cancer_death = cancer_death,
      death = death,
      pys = pys,
      ...
    )
    class(result) <- result_class
    return(result)
  }
)


# Internal finite-difference variance estimator.
calc_delta <- function(
  ages,
  cancer,
  cancer_death,
  death,
  pys,
  age_start = seq(0, 85, 5),
  age_end = 90,
  age_combine = c("cross", "pairwise"),
  risk_func = "devcan",
  base_risk,
  ci_method = "gamma",
  ...
) {
  age_combine <- match.arg(age_combine)
  range <- cross_age_range(
    age_start,
    age_end,
    use_cross = age_combine == "cross"
  )
  dots <- list(...)
  n <- length(cancer)
  risk_length <- length(base_risk)
  delta <- matrix(0, nrow = 3 * n, ncol = risk_length)
  for (l in seq_len(3 * n)) {
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
    if (identical(risk_func, "wun")) {
      pert_risk <- do.call(
        wun_risk_fast,
        c(
          list(
            ages = ages,
            cancer = pert_cancer,
            cancer_death = pert_cancer_death,
            death = pert_death,
            pys = pys,
            age_start = range$start,
            age_end = range$end
          ),
          dots
        )
      )
    } else {
      pert <- calc_risk(
        ages = ages,
        cancer = pert_cancer,
        cancer_death = pert_cancer_death,
        death = pert_death,
        pys = pys,
        risk_func = risk_func,
        ...
      )
      pert_risk <- get_risk(
        pert,
        range$start,
        range$end,
        age_combine = "pairwise"
      )
    }
    delta[l, ] <- pert_risk - base_risk
  }

  z_var <- count_component_variance(
    cancer,
    cancer_death,
    death,
    ci_method
  )

  V <- colSums(delta^2 * z_var)
  V
}

validate_ltr_inputs <- function(
  ages,
  cancer,
  cancer_death,
  death,
  pys,
  alpha,
  age_start,
  age_end,
  multiplier,
  digits,
  return_variance
) {
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
    !is.numeric(alpha) ||
      length(alpha) != 1L ||
      !is.finite(alpha) ||
      alpha <= 0 ||
      alpha >= 1
  ) {
    stop("alpha must be a number between 0 and 1")
  }
  if (
    !is.numeric(age_start) ||
      length(age_start) == 0L ||
      any(!is.finite(age_start))
  ) {
    stop("age_start must be a non-empty vector of finite numbers")
  }
  if (
    !is.numeric(age_end) ||
      length(age_end) == 0L ||
      any(is.na(age_end)) ||
      any(age_end == -Inf)
  ) {
    stop(
      "age_end must be a non-empty numeric vector containing finite values or Inf"
    )
  }
  if (
    !is.numeric(multiplier) ||
      length(multiplier) != 1L ||
      !is.finite(multiplier) ||
      multiplier < 0
  ) {
    stop("multiplier must be a non-negative finite number")
  }
  if (
    !is.numeric(digits) ||
      length(digits) != 1L ||
      !is.finite(digits) ||
      digits < 0 ||
      digits != as.integer(digits)
  ) {
    stop("digits must be a non-negative whole number")
  }
  if (
    !is.logical(return_variance) ||
      length(return_variance) != 1L ||
      is.na(return_variance)
  ) {
    stop("return_variance must be TRUE or FALSE")
  }
}

# Internal gamma upper-limit candidate search.
find_max_risk <- function(
  ages,
  cancer,
  cancer_death,
  death,
  pys,
  age_start = seq(0, 85, 5),
  age_end = 90,
  age_combine = c("cross", "pairwise"),
  risk_func = "devcan",
  base_risk,
  ...
) {
  age_combine <- match.arg(age_combine)
  range <- cross_age_range(
    age_start,
    age_end,
    use_cross = age_combine == "cross"
  )
  n <- length(cancer)
  max_A <- base_risk
  risk_length <- length(range$start)
  best_candidate <- integer(risk_length)
  candidates <- vector("list", 6 * n)
  candidate_id <- 0L
  dots <- list(...)
  fast_devcan <- identical(risk_func, "devcan") &&
    tail(cancer_death, 1) > 0 &&
    tail(death, 1) > 0
  fast_amp <- identical(risk_func, "amp") &&
    {
      type <- dots$type %||% "developing"
      removal_count <- if (type == "developing") {
        cancer + death - cancer_death
      } else {
        death
      }
      tail(removal_count, 1) > 0
    }
  fast_wun <- identical(risk_func, "wun")
  fast_cumulative <- risk_func %in% c("cumulative", "cumu")
  for (dir in c(1, -1)) {
    for (l in seq_len(3 * n)) {
      candidate_id <- candidate_id + 1L
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
        other_death <- death[m] - cancer_death[m]
        pert_death[m] <- cancer_death[m] + max(0, other_death + dir)
      }

      if (fast_devcan || fast_amp || fast_wun || fast_cumulative) {
        pert_risk <- do.call(
          if (fast_devcan) {
            devcan_risk_fast
          } else if (fast_amp) {
            amp_risk_fast
          } else if (fast_cumulative) {
            cumulative_risk_fast
          } else {
            wun_risk_fast
          },
          c(
            list(
              ages = ages,
              cancer = pert_cancer,
              cancer_death = pert_cancer_death,
              death = pert_death,
              pys = pys,
              age_start = range$start,
              age_end = range$end
            ),
            dots
          )
        )
      } else {
        pert <- calc_risk(
          ages = ages,
          cancer = pert_cancer,
          cancer_death = pert_cancer_death,
          death = pert_death,
          pys = pys,
          risk_func = risk_func,
          ...
        )
        pert_risk <- get_risk(
          pert,
          range$start,
          range$end,
          age_combine = "pairwise"
        )
      }
      # Boundary perturbations can create an unidentified open interval (for
      # example, no terminal deaths but a positive terminal incidence count).
      # Such candidates do not define a finite risk and cannot maximize the
      # gamma upper-limit statistic.
      improved <- !is.na(pert_risk) & is.finite(pert_risk) & pert_risk > max_A
      improved[is.na(improved)] <- FALSE
      if (any(improved)) {
        max_A[improved] <- pert_risk[improved]
        best_candidate[improved] <- candidate_id
        candidates[[candidate_id]] <- list(
          cancer = pert_cancer,
          cancer_death = pert_cancer_death,
          death = pert_death
        )
      }
    }
  }
  original <- list(
    cancer = cancer,
    cancer_death = cancer_death,
    death = death
  )
  selected <- lapply(best_candidate, function(i) {
    if (i == 0L) original else candidates[[i]]
  })
  return(
    list(
      risk = max_A,
      cancer_M = lapply(selected, `[[`, "cancer"),
      cancer_death_M = lapply(selected, `[[`, "cancer_death"),
      death_M = lapply(selected, `[[`, "death")
    )
  )
}

cumulative_risk_fast <- function(
  ages,
  cancer,
  cancer_death,
  death,
  pys,
  age_start,
  age_end,
  type = "developing",
  maj_method = "pmaj",
  pmaj_sub_interval = 0.5,
  last_age_widths = Inf
) {
  geometry <- pmaj_geometry(
    ages,
    maj_method,
    pmaj_sub_interval,
    last_age_widths
  )
  widths <- geometry$widths
  fine_ages <- geometry$ages
  count <- if (type == "developing") cancer else cancer_death
  rate <- drop(geometry$design %*% (count / pys))
  integrated <- widths * rate
  integrated[is.infinite(widths) & rate == 0] <- 0
  cumulative <- c(0, cumsum(integrated))

  range <- cross_age_range(age_start, age_end, use_cross = FALSE)
  start <- match(round(range$start, 6), round(fine_ages, 6))
  terminal_age <- if (is.finite(tail(widths, 1))) {
    tail(fine_ages, 1) + tail(widths, 1)
  } else {
    Inf
  }
  terminal <- is.finite(range$end) &
    is.finite(terminal_age) &
    round(range$end, 6) == round(terminal_age, 6)
  end <- ifelse(
    is.finite(range$end) & !terminal,
    match(round(range$end, 6), round(fine_ages, 6)),
    length(integrated)
  )
  cumulative_rate <- cumulative[end + 1L] - cumulative[start]
  unname(-expm1(-cumulative_rate))
}

wun_risk_fast <- function(
  ages,
  cancer,
  cancer_death,
  death,
  pys,
  age_start,
  age_end,
  ...
) {
  model <- wun(
    ages = ages,
    cancer = cancer,
    cancer_death = cancer_death,
    death = death,
    pys = pys,
    ...
  )
  get_risk(
    model,
    age_start = age_start,
    age_end = age_end,
    age_combine = "pairwise"
  )
}

amp_risk_fast <- function(
  ages,
  cancer,
  cancer_death,
  death,
  pys,
  age_start,
  age_end,
  type = "developing",
  maj_method = "pmaj",
  pmaj_sub_interval = 0.5,
  last_age_widths = Inf
) {
  geometry <- pmaj_geometry(
    ages,
    maj_method,
    pmaj_sub_interval,
    last_age_widths
  )
  design <- geometry$design
  widths <- geometry$widths
  fine_ages <- geometry$ages
  event_count <- if (type == "developing") cancer else cancer_death
  removal_count <- if (type == "developing") {
    cancer + death - cancer_death
  } else {
    death
  }
  event_rate <- drop(design %*% (event_count / pys))
  removal_rate <- drop(design %*% (removal_count / pys))
  finite <- is.finite(widths)
  survival <- c(1, exp(-cumsum(widths[finite] * removal_rate[finite])))
  full_factor <- vapply(
    seq_along(widths),
    function(i) integrated_hazard_factor(removal_rate[i], widths[i])$value,
    numeric(1)
  )
  full_contribution <- event_rate * survival * full_factor
  cumulative <- c(0, cumsum(full_contribution))

  range <- cross_age_range(age_start, age_end, use_cross = FALSE)
  start <- match(round(range$start, 6), round(fine_ages, 6))
  terminal_age <- if (is.finite(tail(widths, 1))) {
    tail(fine_ages, 1) + tail(widths, 1)
  } else {
    Inf
  }
  terminal <- is.finite(range$end) &
    is.finite(terminal_age) &
    round(range$end, 6) == round(terminal_age, 6)
  end <- ifelse(
    is.finite(range$end) & !terminal,
    match(round(range$end, 6), round(fine_ages, 6)) - 1L,
    length(fine_ages)
  )
  numerator <- cumulative[end + 1L] - cumulative[start]
  unname(numerator / survival[start])
}

devcan_risk_fast <- function(
  ages,
  cancer,
  cancer_death,
  death,
  pys,
  age_start,
  age_end,
  type = "developing",
  no_other_death = FALSE,
  maj_method = "pmaj",
  pmaj_sub_interval = 0.5
) {
  geometry <- pmaj_geometry(ages, maj_method, pmaj_sub_interval)
  design <- geometry$design
  widths <- geometry$widths
  fine_ages <- geometry$ages
  event_count <- if (type == "developing") cancer else cancer_death
  other_count <- if (no_other_death) {
    numeric(length(death))
  } else {
    death - cancer_death
  }
  l_c <- drop(design %*% (event_count / pys))
  l_d <- drop(design %*% (cancer_death / pys))
  l_o <- drop(design %*% (other_count / pys))
  finite <- is.finite(widths)

  survival_d <- c(1, exp(-cumsum(widths[finite] * l_d[finite])))
  survival_o <- c(1, exp(-cumsum(widths[finite] * l_o[finite])))
  survival <- survival_d * survival_o
  factor_a <- vapply(
    seq_along(widths),
    function(i) integrated_hazard_factor(l_d[i] + l_o[i], widths[i])$value,
    numeric(1)
  )
  factor_d <- vapply(
    seq_along(widths),
    function(i) integrated_hazard_factor(l_d[i], widths[i])$value,
    numeric(1)
  )
  contrib_a <- l_c * survival * factor_a
  contrib_d <- l_c * survival_d * factor_d
  s_c <- c(1, 1 - cumsum(contrib_d))

  range <- cross_age_range(age_start, age_end, use_cross = FALSE)
  start <- match(round(range$start, 6), round(fine_ages, 6))
  end <- ifelse(
    is.finite(range$end),
    match(round(range$end, 6), round(fine_ages, 6)) - 1L,
    length(contrib_a)
  )
  cumulative <- c(0, cumsum(contrib_a))
  numerator <- cumulative[end + 1L] - cumulative[start]
  unname(numerator / (survival_o[start] * s_c[start]))
}
