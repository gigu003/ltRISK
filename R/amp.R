#' Compute probabilities used for estimation of lifetime risk
#'
#' Estimates the lifetime and age-conditional probabilities of
#' developing cancer, adjusted for multiple primary cancers, using a competing
#' risks framework based on the method described in Sasieni et al. (2011).
#'
#' @param ages Starting ages of each age interval (e.g., 0, 5, 10, ..., 85).
#' @param cancer Number of cancer diagnoses in each age interval. For AMP,
#'      this may include multiple primary cancers.
#' @param death Number of all deaths (all causes combined) in each age interval.
#' @param cancer_death Number of deaths due to cancer in each age interval.
#' @param pys Person-years at risk corresponding to each age interval.
#' @param last_age_widths Width of the last age group (default = Inf).
#' @param type Characters "developing" or "dying" indicate estimate the
#'      probability of developing cancer or dying from it.
#' @param maj_method Character string. The method to use for smoothing the
#'      rates: "pmaj" for Piece-wise Mid-Age Group Joinpoint,
#'      "constant" for simple piece-wise constant. Default is "pmaj".
#'      Note: To approximate the exact MAJ (Mid-Age Group Joinpoint)
#'      method, use "pmaj" with a small value for \code{pmaj_sub_interval}
#'      (e.g., 0.01), as MAJ requires numerical integration
#'      for exact computation but can be closely approximated this way.
#' @param pmaj_sub_interval Numeric. Sub-interval size for pmaj (default =
#'      0.5 years).
#'
#' @returns Contribution of risk of developing cancer or dying from it in each
#'      age group.
#'
#' @export
#'
#' @references
#' Sasieni PD, Shelton J, Ormiston-Smith N, Thomson CS, Silcocks PB. What is
#' the lifetime risk of developing cancer?: the effect of adjusting for
#' multiple primaries. \emph{Br J Cancer}, 2011;105:460–465.
#' DOI: 10.1038/bjc.2011.250
#'
amp <- function(
  ages,
  cancer,
  cancer_death,
  death,
  pys,
  last_age_widths = Inf,
  type = "developing",
  maj_method = "pmaj",
  pmaj_sub_interval = 0.5
) {
  maj_method <- match.arg(maj_method, c("pmaj", "constant"))
  type <- validate_model_inputs(
    ages,
    cancer,
    cancer_death,
    death,
    pys,
    type,
    last_age_widths
  )
  if (type == "dying") {
    cancer <- cancer_death
  }
  event_rate <- cancer / pys
  removal_rate <- (cancer + death - cancer_death) / pys
  event_grid <- pmaj_grid(
    ages,
    event_rate,
    maj_method = maj_method,
    pmaj_sub_interval = pmaj_sub_interval,
    last_age_widths = last_age_widths
  )
  removal_grid <- pmaj_grid(
    ages,
    removal_rate,
    maj_method = maj_method,
    pmaj_sub_interval = pmaj_sub_interval,
    last_age_widths = last_age_widths
  )
  fine_event_rate <- event_grid$rate
  fine_removal_rate <- removal_grid$rate
  fine_deltas <- event_grid$widths
  fine_ages <- event_grid$ages
  # Set starting point to the first age
  start_idx <- 1
  # No previous cumulative removal
  prev_cum_removal <- 0
  S0_cond <- exp(-prev_cum_removal)
  # Initialize integral and cumulative removal from first age
  integral <- 0
  cum_removal_from_cond <- 0
  # Loop over all bands
  n_fine <- length(fine_ages)
  ages_out <- fine_ages
  S0s <- numeric(n_fine)
  contribs <- numeric(n_fine)
  fracs <- numeric(n_fine)
  rem_rates <- fine_removal_rate
  widths_out <- fine_deltas
  for (i in start_idx:n_fine) {
    # S0_star at a_i
    S0_i <- S0_cond * exp(-cum_removal_from_cond)
    # Fraction: event rate / removal rate
    frac <- ifelse(
      fine_removal_rate[i] == 0,
      0,
      fine_event_rate[i] / fine_removal_rate[i]
    )
    # Effective width for this band (full width)
    w_i <- fine_deltas[i]
    # If effective width is zero or negative, skip
    if (w_i <= 0) {
      next
    }
    S0s[i] <- S0_i
    # Removal rate for this band
    rem_rate_i <- fine_removal_rate[i]
    # exp_term
    if (is.finite(w_i)) {
      exp_term <- 1 - exp(-w_i * rem_rate_i)
    } else {
      exp_term <- 1
    }
    # Contribution
    contrib <- frac * S0_i * exp_term
    integral <- integral + contrib
    contribs[i] <- contrib
    fracs[i] <- frac
    # Update cumulative removal (only if finite width)
    if (is.finite(w_i)) {
      cum_removal_from_cond <- cum_removal_from_cond + w_i * rem_rate_i
    }
  }
  # Conditional risk = integral / S0_cond
  cond_risk <- ifelse(S0_cond == 0, 0, integral / S0_cond)
  res <- list(
    risk = cond_risk,
    ages = ages_out,
    contrib_a = contribs,
    S0ai = S0s,
    fracs = fracs,
    rem_rates = rem_rates,
    widths = widths_out
  )
  class(res) <- "amp"
  return(res)
}
