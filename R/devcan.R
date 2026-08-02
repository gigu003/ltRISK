#' Compute probabilities used for estimation of lifetime risk
#'
#' @inheritParams amp
#' @param no_other_death Logical. If \code{TRUE}, assumes absence of other
#'      causes of death (i.e., non-cancer mortality = 0). Default = FALSE.
#' @param maj_method Character string. The method to use for smoothing the
#'      rates: "pmaj" for Piece-wise Mid-Age Group Joinpoint,
#'      "constant" for simple piece-wise constant. Default is "pmaj".
#'      Note: To approximate the exact MAJ (Mid-Age Group Joinpoint)
#'      method, use "pmaj" with a small value for \code{pmaj_sub_interval}
#'      (e.g., 0.01), as MAJ requires numerical integration
#'      for exact computation but can be closely approximated this way.
#' @param pmaj_sub_interval Numeric. Sub-interval size for pmaj (default =
#'      0.5 years).
#' @return Contribution of risk of developing cancer or dying from it in each
#'      age group.
#'
#' @export
#'
#' @references
#'
#' Fay MP, Pfeiffer R, Cronin KA, Le C, Feuer EJ. (2003). *Age-conditional*
#' *probabilities of developing cancer*. \emph{Statistics in Medicine},
#' 22(11):1837-1848. DOI: 10.1002/sim.1428.
#'
#' Fay M P. *Estimating age conditional probability of developing disease*
#' *from surveillance data\[J\]*. Population Health Metrics, 2004, 2(1): 6.
#'
devcan <- function(
  ages,
  cancer,
  death,
  cancer_death,
  pys,
  type = "developing",
  no_other_death = FALSE,
  maj_method = "pmaj",
  pmaj_sub_interval = 0.5
) {
  type <- validate_model_inputs(
    ages,
    cancer,
    cancer_death,
    death,
    pys,
    type,
    Inf
  )

  # Calculate the probability of dying from cancer
  if (type == "dying") {
    cancer <- cancer_death
  }

  # Absence of other causes of death
  if (no_other_death) {
    death <- cancer_death
  }

  # Calculate the number of death by other causes
  non_cancer_death <- death - cancer_death
  l_c <- cancer / pys
  l_d <- cancer_death / pys
  l_o <- non_cancer_death / pys
  grid_c <- pmaj_grid(ages, l_c, maj_method, pmaj_sub_interval)
  grid_d <- pmaj_grid(ages, l_d, maj_method, pmaj_sub_interval)
  grid_o <- pmaj_grid(ages, l_o, maj_method, pmaj_sub_interval)
  fine_l_c <- grid_c$rate
  fine_l_d <- grid_d$rate
  fine_l_o <- grid_o$rate
  fine_deltas <- grid_c$widths
  fine_ages <- grid_c$ages

  n_intervals <- length(fine_ages)
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

  if (any(s_c > 1)) {
    warning(
      "Impossible cohort detected; check data for zero deaths in oldest groups or inconsistencies."
    )
  }

  res <- list(
    ages = fine_ages,
    contrib_a = contrib_a,
    s = s,
    s_o = s_o,
    s_c = s_c
  )
  class(res) <- "devcan"
  return(res)
}
