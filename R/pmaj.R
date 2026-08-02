#' Smooth a Single Rate Using Piecewise Mid-Age Group Joinpoint (PMAJ) Method
#'
#' This function implements the Piecewise Mid-Age Group Joinpoint (PMAJ)
#' smoothing method to approximate a smoothed rate (e.g., cancer incidence)
#' over finer sub-intervals. It uses linear interpolation between midpoints
#' of original age intervals to create piecewise constant rates on a finer
#' grid.
#'
#' @param ages Starting ages of the original age intervals
#'      (e.g., 0, 5, 10, ..., 95).
#' @param rate_hat Estimated rates for the original intervals
#'      (e.g., cancer incidence rates).
#' @param maj_method The method to use for smoothing the rates,
#'    default is "pmaj".
#'  - "pmaj" for Piecewise Mid-Age Group Joinpoint;
#'  - "constant" for simple piecewise constant.
#'  Note: To approximate the exact MAJ (Mid-Age Group Joinpoint) method,
#'  use "pmaj" with a small value for \code{pmaj_sub_interval} (e.g., 0.01),
#'  as MAJ requires numerical integration for exact computation but can be
#'  closely approximated this way.
#' @param pmaj_sub_interval The width of sub-intervals for the PMAJ
#'      approximation (default = 0.5 years).
#'
#' @returns Numeric vector of smoothed rates on the fine grid. When an original
#'   age interval is not evenly divisible by `pmaj_sub_interval`, the final
#'   sub-interval is shortened so that the original age boundaries are retained.
#'
#' @importFrom stats qnorm
#' @importFrom utils tail
#'
#' @references
#' Fay MP. Estimating age conditional probability of developing disease from
#' surveillance data. Popul Health Metr. 2004 Jul 27;2(1):6.
#' doi: 10.1186/1478-7954-2-6. PMID: 15279675; PMCID: PMC517510.
#'
#'
#' @export
#'
#' @examples
#'
#' ages <- seq(0, 95, 5)
#' cancer <- c(0, 0, 1, 9, 43, 335, 1116, 2670, 5183, 7392, 8012, 7341, 7010,
#' 7651, 8060, 7146, 4754, 2574, 952, 273)
#' pys <- c(4052953, 4032790, 3784789, 3810986, 3675646, 4138795, 4575728,
#' 4831799, 4578168, 3906260, 3054146, 2353577, 1981443, 1988371, 1838556,
#' 1541002, 1083867, 629172, 299128, 114178)
#' rate_hat_c <- cancer / pys
#' pmaj(ages, rate_hat_c)
#' pmaj(ages, rate_hat_c, maj_method = "constant")
#' # To approximate MAJ, use small sub-interval with PMAJ
#' pmaj(ages, rate_hat_c, pmaj_sub_interval = 0.01)
#'
pmaj <- function(ages, rate_hat, maj_method = "pmaj", pmaj_sub_interval = 0.5) {
  pmaj_grid(
    ages = ages,
    rate_hat = rate_hat,
    maj_method = maj_method,
    pmaj_sub_interval = pmaj_sub_interval
  )$rate
}

pmaj_grid <- function(
  ages,
  rate_hat,
  maj_method = "pmaj",
  pmaj_sub_interval = 0.5,
  last_age_widths = Inf
) {
  maj_method <- match.arg(maj_method, c("pmaj", "constant"))
  validate_rate_inputs(ages, rate_hat)
  n_intervals <- length(ages)
  geometry <- pmaj_geometry(
    ages,
    maj_method,
    pmaj_sub_interval,
    last_age_widths
  )
  geometry$rate <- drop(geometry$design %*% rate_hat)
  geometry
}

pmaj_geometry <- memoise::memoise(function(
  ages,
  maj_method = "pmaj",
  pmaj_sub_interval = 0.5,
  last_age_widths = Inf
) {
  if (
    !is.numeric(last_age_widths) ||
      length(last_age_widths) != 1L ||
      is.na(last_age_widths) ||
      last_age_widths <= 0
  ) {
    stop("last_age_widths must be a positive number or Inf")
  }
  n_intervals <- length(ages)
  if (maj_method == "constant") {
    return(list(
      ages = ages,
      widths = calc_widths(ages, last_age_widths),
      design = diag(n_intervals)
    ))
  }
  if (
    !is.numeric(pmaj_sub_interval) ||
      length(pmaj_sub_interval) != 1L ||
      !is.finite(pmaj_sub_interval) ||
      pmaj_sub_interval <= 0
  ) {
    stop("pmaj_sub_interval must be a positive finite number")
  }

  widths <- diff(ages)
  if (length(widths) == 0) {
    widths <- 5
  }
  widths <- c(
    widths,
    if (is.finite(last_age_widths)) last_age_widths else tail(widths, 1)
  )
  t_mid <- ages + widths / 2
  interval_ends <- c(ages[-1], tail(t_mid, 1))
  boundaries <- sort(unique(unlist(
    Map(
      function(start, end) {
        grid <- seq(start, end, by = pmaj_sub_interval)
        if (tail(grid, 1) < end) {
          grid <- c(grid, end)
        }
        grid
      },
      ages,
      interval_ends
    ),
    use.names = FALSE
  )))
  if (length(boundaries) < 2L) {
    stop("Unable to construct a PMAJ grid")
  }

  boundary_design <- vapply(
    seq_len(n_intervals),
    function(i) {
      stats::approx(
        x = t_mid,
        y = as.numeric(seq_len(n_intervals) == i),
        xout = boundaries,
        method = "linear",
        rule = 2,
        ties = "ordered"
      )$y
    },
    numeric(length(boundaries))
  )
  finite_design <- (head(boundary_design, -1L) + tail(boundary_design, -1L)) / 2
  list(
    ages = boundaries,
    widths = c(diff(boundaries), last_age_widths),
    design = rbind(finite_design, diag(n_intervals)[n_intervals, ])
  )
})
