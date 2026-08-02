#' Calculate cumulative rate and risk.
#'
#' This function computes age-specific rates and cumulative risk contributions
#' across age intervals based on cancer incidence or cancer mortality counts.
#' It converts interval counts into rates using person-years, and then calculates
#' the contribution of each age group to the cumulative risk using
#' \eqn{1 - \exp(-r_x \cdot n_x)}, where \eqn{r_x} is the age-specific rate and
#' \eqn{n_x} is the width of the age interval.
#'
#' @inheritParams amp
#' @param maj_method Character string. The method to use for smoothing the
#'      rates: "pmaj" for Piece-wise Mid-Age Group Joinpoint,
#'      "constant" for simple piece-wise constant. Default is "pmaj".
#'      Note: To approximate the exact MAJ (Mid-Age Group Joinpoint)
#'      method, use "pmaj" with a small value for \code{pmaj_sub_interval}
#'      (e.g., 0.01), as MAJ requires numerical integration
#'      for exact computation but can be closely approximated this way.
#' @param pmaj_sub_interval Numeric. Sub-interval size for pmaj (default =
#'      0.5 years).
#' @returns
#' A list of class \code{"cumu"} containing:
#' \describe{
#'   \item{ages}{Vector of starting ages.}
#'   \item{widths}{Vector of widths for each age interval.}
#'   \item{rate}{Age-specific rates, computed as \code{count / pys}.}
#' }
#'
#' @seealso \code{\link{get_risk.cumu}} for obtaining cumulative risks across
#'   arbitrary age ranges.
#' @export
#'
cumulative <- function(
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
  type <- validate_model_inputs(
    ages,
    cancer,
    cancer_death,
    death,
    pys,
    type,
    last_age_widths
  )
  if (type == "developing") {
    count <- cancer
  } else if (type == "dying") {
    count <- cancer_death
  }
  rate <- count / pys

  grid <- pmaj_grid(
    ages,
    rate,
    maj_method,
    pmaj_sub_interval,
    last_age_widths
  )
  fine_rate <- grid$rate
  fine_deltas <- grid$widths
  fine_ages <- grid$ages

  res <- list(
    ages = fine_ages,
    widths = fine_deltas,
    rate = fine_rate
  )
  class(res) <- c("cumu", class(res))
  return(res)
}
