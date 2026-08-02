calc_variance <- function(
  ages,
  cancer,
  cancer_death,
  death,
  pys,
  age_start,
  age_end,
  age_combine = c("cross", "pairwise"),
  risk_func,
  base_risk,
  ci_method = c("gamma", "delta"),
  variance_method = "auto",
  ...
) {
  age_combine <- match.arg(age_combine)
  ci_method <- match.arg(ci_method)
  variance_method <- match.arg(
    variance_method,
    c("auto", "analytic", "finite_difference")
  )
  dots <- list(...)
  analytic_method <- analytic_variance_method(
    risk_func,
    dots,
    cancer,
    cancer_death,
    death
  )

  if (variance_method == "analytic" && is.null(analytic_method)) {
    stop(
      "Analytic variance is not available for this risk function and rate model"
    )
  }
  if (variance_method == "finite_difference" || is.null(analytic_method)) {
    return(calc_delta(
      ages = ages,
      cancer = cancer,
      cancer_death = cancer_death,
      death = death,
      pys = pys,
      age_start = age_start,
      age_end = age_end,
      age_combine = age_combine,
      risk_func = risk_func,
      base_risk = base_risk,
      ci_method = ci_method,
      ...
    ))
  }

  do.call(
    analytic_method,
    c(
      list(
        ages = ages,
        cancer = cancer,
        cancer_death = cancer_death,
        death = death,
        pys = pys,
        age_start = age_start,
        age_end = age_end,
        age_combine = age_combine,
        ci_method = ci_method
      ),
      dots
    )
  )
}

analytic_variance_method <- function(
  risk_func,
  dots,
  cancer,
  cancer_death,
  death
) {
  risk_func <- if (risk_func == "cumu") "cumulative" else risk_func
  if (risk_func == "devcan") {
    type <- dots$type %||% "developing"
    event_count <- if (type == "developing") cancer else cancer_death
    competing_count <- if (isTRUE(dots$no_other_death)) {
      cancer_death
    } else {
      death
    }
    if (tail(cancer_death, 1) == 0 || tail(competing_count, 1) == 0) {
      return(NULL)
    }
  } else if (risk_func == "amp") {
    type <- dots$type %||% "developing"
    removal_count <- if (type == "developing") {
      cancer + death - cancer_death
    } else {
      death
    }
    if (tail(removal_count, 1) == 0) return(NULL)
  } else if (risk_func == "wun") {
    if (isTRUE(dots$correct_for_surgery)) {
      return(NULL)
    }
    if (!is.finite(dots$last_age_widths %||% Inf) && tail(death, 1) == 0) {
      return(NULL)
    }
  }

  switch(
    risk_func,
    cumulative = calc_variance_cumulative_constant,
    devcan = calc_variance_devcan_constant,
    amp = calc_variance_amp,
    wun = calc_variance_wun,
    NULL
  )
}

calc_variance_wun <- function(
  ages,
  cancer,
  cancer_death,
  death,
  pys,
  age_start,
  age_end,
  age_combine = c("cross", "pairwise"),
  type = "developing",
  cohort_size = 1,
  last_age_widths = Inf,
  correct_for_surgery = FALSE,
  ci_method = c("gamma", "delta"),
  ...
) {
  age_combine <- match.arg(age_combine)
  ci_method <- match.arg(ci_method)
  if (correct_for_surgery) {
    stop("Analytic Wun variance is not available with surgery correction")
  }
  n <- length(ages)
  p <- 3 * n
  widths <- calc_widths(ages, last_age_widths)
  z_var <- count_component_variance(
    cancer,
    cancer_death,
    death,
    ci_method
  )
  event_count <- if (type == "developing") cancer else cancer_death
  other_count <- death - cancer_death

  d_event <- matrix(0, n, p)
  if (type == "developing") {
    d_event[, seq_len(n)] <- diag(1 / pys)
  } else {
    d_event[, n + seq_len(n)] <- diag(1 / pys)
  }
  d_other <- matrix(0, n, p)
  d_other[, 2 * n + seq_len(n)] <- diag(1 / pys)
  d_death <- d_other
  d_death[, n + seq_len(n)] <- diag(1 / pys)

  event_rate <- event_count / pys
  other_rate <- other_count / pys
  death_rate <- death / pys
  l <- l_cf <- numeric(n)
  l[1] <- l_cf[1] <- cohort_size
  d_l <- d_l_cf <- matrix(0, n, p)
  contribution <- numeric(n)
  d_contribution <- matrix(0, n, p)

  if (n > 1L) {
    for (i in seq_len(n - 1L)) {
      interval <- wun_closed_interval_gradient(
        width = widths[i],
        l = l[i],
        l_cf = l_cf[i],
        d_l = d_l[i, ],
        d_l_cf = d_l_cf[i, ],
        event_rate = event_rate[i],
        other_rate = other_rate[i],
        death_rate = death_rate[i],
        d_event = d_event[i, ],
        d_other = d_other[i, ],
        d_death = d_death[i, ],
        type = type
      )
      contribution[i] <- interval$contribution
      d_contribution[i, ] <- interval$d_contribution
      l[i + 1L] <- interval$l_next
      l_cf[i + 1L] <- interval$l_cf_next
      d_l[i + 1L, ] <- interval$d_l_next
      d_l_cf[i + 1L, ] <- interval$d_l_cf_next
    }
  }

  i <- n
  if (is.finite(widths[i])) {
    interval <- wun_closed_interval_gradient(
      width = widths[i],
      l = l[i],
      l_cf = l_cf[i],
      d_l = d_l[i, ],
      d_l_cf = d_l_cf[i, ],
      event_rate = event_rate[i],
      other_rate = other_rate[i],
      death_rate = death_rate[i],
      d_event = d_event[i, ],
      d_other = d_other[i, ],
      d_death = d_death[i, ],
      type = type
    )
    contribution[i] <- interval$contribution
    d_contribution[i, ] <- interval$d_contribution
  } else {
    open <- wun_open_interval_gradient(
      l = l[i],
      l_cf = l_cf[i],
      d_l = d_l[i, ],
      d_l_cf = d_l_cf[i, ],
      event_rate = event_rate[i],
      other_rate = other_rate[i],
      death_rate = death_rate[i],
      d_event = d_event[i, ],
      d_other = d_other[i, ],
      d_death = d_death[i, ]
    )
    contribution[i] <- open$contribution
    d_contribution[i, ] <- open$d_contribution
  }

  range <- cross_age_range(
    age_start,
    age_end,
    use_cross = age_combine == "cross"
  )
  gradient <- matrix(0, p, nrow(range))
  terminal_age <- tail(ages, 1) + last_age_widths
  for (k in seq_len(nrow(range))) {
    start <- match2(range$start[k], ages)
    terminal <- is.finite(terminal_age) &&
      round(range$end[k], 6) == round(terminal_age, 6)
    end <- if (is.finite(range$end[k]) && !terminal) {
      match2(range$end[k], ages) - 1L
    } else {
      n
    }
    idx <- seq.int(start, end)
    numerator <- sum(contribution[idx])
    d_numerator <- colSums(d_contribution[idx, , drop = FALSE])
    gradient[, k] <- d_numerator /
      l_cf[start] -
      numerator * d_l_cf[start, ] / l_cf[start]^2
  }
  variance_from_gradient(gradient, z_var)
}

wun_closed_interval_gradient <- function(
  width,
  l,
  l_cf,
  d_l,
  d_l_cf,
  event_rate,
  other_rate,
  death_rate,
  d_event,
  d_other,
  d_death,
  type
) {
  prevalence <- if (type == "dying") 1 else l / l_cf
  d_prevalence <- if (type == "dying") {
    numeric(length(d_l))
  } else {
    d_l / l_cf - l * d_l_cf / l_cf^2
  }
  event_probability <- -expm1(-width * event_rate)
  d_event_probability <- width * exp(-width * event_rate) * d_event
  adjusted_probability <- event_probability * prevalence
  d_adjusted_probability <-
    prevalence * d_event_probability + event_probability * d_prevalence
  adjusted_rate <- -log1p(-adjusted_probability) / width
  d_adjusted_rate <- d_adjusted_probability /
    (width * (1 - adjusted_probability))
  combined_rate <- other_rate + adjusted_rate
  d_combined_rate <- d_other + d_adjusted_rate
  exit_probability <- -expm1(-width * combined_rate)
  d_exit_probability <- width * exp(-width * combined_rate) * d_combined_rate

  if (combined_rate == 0) {
    contribution <- 0
    d_contribution <- l_cf * width * d_adjusted_rate
    exits <- 0
    d_exits <- l_cf * width * d_combined_rate
  } else {
    allocation <- adjusted_rate / combined_rate
    d_allocation <- d_adjusted_rate /
      combined_rate -
      adjusted_rate * d_combined_rate / combined_rate^2
    contribution <- l_cf * exit_probability * allocation
    d_contribution <-
      d_l_cf *
      exit_probability *
      allocation +
      l_cf * d_exit_probability * allocation +
      l_cf * exit_probability * d_allocation
    exits <- l_cf * exit_probability
    d_exits <- d_l_cf * exit_probability + l_cf * d_exit_probability
  }
  survival <- exp(-width * death_rate)
  list(
    contribution = contribution,
    d_contribution = d_contribution,
    l_next = l * survival,
    d_l_next = survival * (d_l - l * width * d_death),
    l_cf_next = l_cf - exits,
    d_l_cf_next = d_l_cf - d_exits
  )
}

wun_open_interval_gradient <- function(
  l,
  l_cf,
  d_l,
  d_l_cf,
  event_rate,
  other_rate,
  death_rate,
  d_event,
  d_other,
  d_death
) {
  ratio <- l / (death_rate * l_cf)
  d_ratio <- d_l /
    (death_rate * l_cf) -
    l * d_death / (death_rate^2 * l_cf) -
    l * d_l_cf / (death_rate * l_cf^2)
  omega <- ratio * event_rate
  d_omega <- event_rate * d_ratio + ratio * d_event
  denominator <- 1 - omega
  adjusted_rate <- omega * other_rate / denominator
  d_adjusted_rate <-
    other_rate * d_omega / denominator^2 + omega * d_other / denominator
  combined_rate <- adjusted_rate + other_rate
  d_combined_rate <- d_adjusted_rate + d_other
  allocation <- adjusted_rate / combined_rate
  d_allocation <- d_adjusted_rate /
    combined_rate -
    adjusted_rate * d_combined_rate / combined_rate^2
  list(
    contribution = l_cf * allocation,
    d_contribution = d_l_cf * allocation + l_cf * d_allocation
  )
}

calc_variance_amp <- function(
  ages,
  cancer,
  cancer_death,
  death,
  pys,
  age_start,
  age_end,
  age_combine = c("cross", "pairwise"),
  type = "developing",
  maj_method = "pmaj",
  pmaj_sub_interval = 0.5,
  last_age_widths = Inf,
  ci_method = c("gamma", "delta"),
  ...
) {
  age_combine <- match.arg(age_combine)
  ci_method <- match.arg(ci_method)
  n <- length(ages)
  p <- 3 * n
  geometry <- pmaj_geometry(
    ages,
    maj_method,
    pmaj_sub_interval,
    last_age_widths
  )
  design <- geometry$design
  fine_ages <- geometry$ages
  widths <- geometry$widths
  m <- length(fine_ages)
  z_var <- count_component_variance(
    cancer,
    cancer_death,
    death,
    ci_method
  )

  event_count <- if (type == "developing") cancer else cancer_death
  removal_count <- if (type == "developing") {
    cancer + death - cancer_death
  } else {
    death
  }
  event_rate <- drop(design %*% (event_count / pys))
  removal_rate <- drop(design %*% (removal_count / pys))
  scaled_design <- sweep(design, 2, pys, "/")

  d_event <- matrix(0, m, p)
  d_removal <- matrix(0, m, p)
  if (type == "developing") {
    d_event[, seq_len(n)] <- scaled_design
    d_removal[, seq_len(n)] <- scaled_design
  } else {
    d_event[, n + seq_len(n)] <- scaled_design
    d_removal[, n + seq_len(n)] <- scaled_design
  }
  d_removal[, 2 * n + seq_len(n)] <- scaled_design

  survival <- numeric(m)
  survival[1] <- 1
  d_survival <- matrix(0, m, p)
  if (m > 1L) {
    for (i in seq_len(m - 1L)) {
      multiplier <- exp(-widths[i] * removal_rate[i])
      survival[i + 1L] <- survival[i] * multiplier
      d_survival[i + 1L, ] <- multiplier *
        (d_survival[i, ] - survival[i] * widths[i] * d_removal[i, ])
    }
  }

  range <- cross_age_range(
    age_start,
    age_end,
    use_cross = age_combine == "cross"
  )
  gradient <- matrix(0, p, nrow(range))
  for (k in seq_len(nrow(range))) {
    start <- match2(range$start[k], fine_ages)
    if (is.na(start)) {
      stop("Some age_start not found in ages")
    }
    end_age <- range$end[k]
    denominator <- survival[start]
    d_denominator <- d_survival[start, ]
    risk <- 0
    d_risk <- numeric(p)

    for (i in seq.int(start, m)) {
      if (fine_ages[i] >= end_age) {
        break
      }
      band_end <- fine_ages[i] + widths[i]
      effective_end <- min(band_end, end_age)
      effective_width <- effective_end - fine_ages[i]
      if (effective_width <= 0) {
        next
      }
      factor <- integrated_hazard_factor(removal_rate[i], effective_width)
      ratio <- survival[i] / denominator
      d_ratio <- d_survival[i, ] /
        denominator -
        survival[i] * d_denominator / denominator^2
      contribution <- event_rate[i] * ratio * factor$value
      d_contribution <-
        ratio *
        factor$value *
        d_event[i, ] +
        event_rate[i] * factor$value * d_ratio +
        event_rate[i] * ratio * factor$derivative * d_removal[i, ]
      risk <- risk + contribution
      d_risk <- d_risk + d_contribution
      if (effective_end < band_end) break
    }
    gradient[, k] <- d_risk
  }

  variance_from_gradient(gradient, z_var)
}

calc_variance_cumulative_constant <- function(
  ages,
  cancer,
  cancer_death,
  death,
  pys,
  age_start,
  age_end,
  age_combine = c("cross", "pairwise"),
  type = "developing",
  last_age_widths = Inf,
  maj_method = "constant",
  pmaj_sub_interval = 0.5,
  ci_method = c("gamma", "delta"),
  ...
) {
  age_combine <- match.arg(age_combine)
  ci_method <- match.arg(ci_method)
  range <- cross_age_range(
    age_start,
    age_end,
    use_cross = age_combine == "cross"
  )
  grid <- pmaj_grid(
    ages,
    rep(0, length(ages)),
    maj_method = maj_method,
    pmaj_sub_interval = pmaj_sub_interval,
    last_age_widths = last_age_widths
  )
  widths <- grid$widths
  design <- grid$design
  count <- if (type == "developing") cancer else cancer_death
  z_var <- count_component_variance(
    cancer,
    cancer_death,
    death,
    ci_method
  )
  n <- length(ages)
  gradient <- matrix(0, nrow = 3 * n, ncol = nrow(range))
  offset <- if (type == "developing") 0L else n

  for (k in seq_len(nrow(range))) {
    start <- match2(range$start[k], grid$ages)
    terminal_age <- if (is.finite(tail(widths, 1))) {
      tail(grid$ages, 1) + tail(widths, 1)
    } else {
      Inf
    }
    terminal <- is.finite(range$end[k]) &&
      is.finite(terminal_age) &&
      round(range$end[k], 6) == round(terminal_age, 6)
    end <- if (is.finite(range$end[k]) && !terminal) {
      match2(range$end[k], grid$ages)
    } else {
      length(widths)
    }
    idx <- seq.int(start, end)
    original_rate <- count / pys
    fine_rate <- drop(design[idx, , drop = FALSE] %*% original_rate)
    if (any(!is.finite(widths[idx]) & fine_rate > 0)) {
      next
    }
    rate_contribution <- widths[idx] * fine_rate
    rate_contribution[widths[idx] == Inf & fine_rate == 0] <- 0
    cumulative_rate <- sum(rate_contribution)
    survival <- exp(-cumulative_rate)
    if (survival == 0) {
      gradient[offset + seq_len(n), k] <- 0
    } else {
      weighted_design <- design[idx, , drop = FALSE] * widths[idx]
      weighted_design[
        !is.finite(weighted_design) & design[idx, , drop = FALSE] == 0
      ] <- 0
      d_integral <- colSums(weighted_design) / pys
      gradient[offset + seq_len(n), k] <- survival * d_integral
    }
  }

  variance_from_gradient(gradient, z_var)
}

calc_variance_devcan_constant <- function(
  ages,
  cancer,
  cancer_death,
  death,
  pys,
  age_start,
  age_end,
  age_combine = c("cross", "pairwise"),
  type = "developing",
  no_other_death = FALSE,
  maj_method = "constant",
  pmaj_sub_interval = 0.5,
  ci_method = c("gamma", "delta"),
  ...
) {
  age_combine <- match.arg(age_combine)
  ci_method <- match.arg(ci_method)
  n <- length(ages)
  p <- 3 * n
  grid <- pmaj_grid(ages, rep(0, n), maj_method, pmaj_sub_interval)
  widths <- grid$widths
  fine_ages <- grid$ages
  design <- grid$design
  m <- length(fine_ages)
  z_var <- count_component_variance(
    cancer,
    cancer_death,
    death,
    ci_method
  )

  original_l_c <- if (type == "developing") cancer / pys else cancer_death / pys
  original_l_d <- cancer_death / pys
  original_l_o <- if (no_other_death) {
    numeric(n)
  } else {
    (death - cancer_death) / pys
  }
  l_c <- drop(design %*% original_l_c)
  l_d <- drop(design %*% original_l_d)
  l_o <- drop(design %*% original_l_o)

  d_l_c <- matrix(0, m, p)
  d_l_d <- matrix(0, m, p)
  d_l_o <- matrix(0, m, p)
  if (type == "developing") {
    d_l_c[, seq_len(n)] <- sweep(design, 2, pys, "/")
  } else {
    d_l_c[, n + seq_len(n)] <- sweep(design, 2, pys, "/")
  }
  d_l_d[, n + seq_len(n)] <- sweep(design, 2, pys, "/")
  if (!no_other_death) {
    d_l_o[, 2 * n + seq_len(n)] <- sweep(design, 2, pys, "/")
  }

  s_d <- s_o <- rep(NA_real_, m + 1L)
  s_c <- rep(NA_real_, m + 1L)
  s_d[1] <- s_o[1] <- s_c[1] <- 1
  d_s_d <- d_s_o <- d_s_c <- matrix(0, m + 1L, p)
  contrib_a <- numeric(m)
  d_contrib_a <- matrix(0, m, p)

  for (i in seq_len(m)) {
    fd <- integrated_hazard_factor(l_d[i], widths[i])
    contrib_d <- l_c[i] * s_d[i] * fd$value
    d_contrib_d <-
      s_d[i] *
      fd$value *
      d_l_c[i, ] +
      l_c[i] * fd$value * d_s_d[i, ] +
      l_c[i] * s_d[i] * fd$derivative * d_l_d[i, ]
    s_c[i + 1L] <- s_c[i] - contrib_d
    d_s_c[i + 1L, ] <- d_s_c[i, ] - d_contrib_d

    l_a <- l_d[i] + l_o[i]
    d_l_a <- d_l_d[i, ] + d_l_o[i, ]
    fa <- integrated_hazard_factor(l_a, widths[i])
    s <- s_d[i] * s_o[i]
    d_s <- s_o[i] * d_s_d[i, ] + s_d[i] * d_s_o[i, ]
    contrib_a[i] <- l_c[i] * s * fa$value
    d_contrib_a[i, ] <-
      s *
      fa$value *
      d_l_c[i, ] +
      l_c[i] * fa$value * d_s +
      l_c[i] * s * fa$derivative * d_l_a

    if (is.finite(widths[i])) {
      exp_d <- exp(-l_d[i] * widths[i])
      exp_o <- exp(-l_o[i] * widths[i])
      s_d[i + 1L] <- s_d[i] * exp_d
      s_o[i + 1L] <- s_o[i] * exp_o
      d_s_d[i + 1L, ] <- exp_d *
        (d_s_d[i, ] - s_d[i] * widths[i] * d_l_d[i, ])
      d_s_o[i + 1L, ] <- exp_o *
        (d_s_o[i, ] - s_o[i] * widths[i] * d_l_o[i, ])
    } else {
      s_d[i + 1L] <- s_o[i + 1L] <- 0
      d_s_d[i + 1L, ] <- d_s_o[i + 1L, ] <- 0
    }
  }

  range <- cross_age_range(
    age_start,
    age_end,
    use_cross = age_combine == "cross"
  )
  gradient <- matrix(0, nrow = p, ncol = nrow(range))
  for (k in seq_len(nrow(range))) {
    start <- match2(range$start[k], fine_ages)
    end <- if (is.finite(range$end[k])) {
      match2(range$end[k], fine_ages) - 1L
    } else {
      m
    }
    numerator <- sum(contrib_a[seq.int(start, end)])
    d_numerator <- colSums(d_contrib_a[seq.int(start, end), , drop = FALSE])
    denominator <- s_o[start] * s_c[start]
    d_denominator <-
      s_c[start] * d_s_o[start, ] + s_o[start] * d_s_c[start, ]
    gradient[, k] <-
      d_numerator / denominator - numerator * d_denominator / denominator^2
  }

  variance_from_gradient(gradient, z_var)
}

integrated_hazard_factor <- function(rate, width) {
  if (!is.finite(width)) {
    return(list(value = 1 / rate, derivative = -1 / rate^2))
  }
  if (rate == 0) {
    return(list(value = width, derivative = -(width^2) / 2))
  }
  exp_term <- exp(-rate * width)
  value <- -expm1(-rate * width) / rate
  derivative <- (rate * width * exp_term - (1 - exp_term)) / rate^2
  list(value = value, derivative = derivative)
}

variance_from_gradient <- function(gradient, z_var) {
  stochastic <- z_var > 0
  if (!any(stochastic)) {
    return(numeric(ncol(gradient)))
  }
  colSums(gradient[stochastic, , drop = FALSE]^2 * z_var[stochastic])
}

count_component_variance <- function(
  cancer,
  cancer_death,
  death,
  ci_method = c("gamma", "delta")
) {
  ci_method <- match.arg(ci_method)
  variance <- c(cancer, cancer_death, death - cancer_death)
  if (ci_method == "delta") {
    variance[variance == 0] <- 0.5
  }
  variance
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
