#' Example Cancer Data from SEER
#'
#'
#' @format A data frame with 40 rows and 7 variables:
#' \describe{
#'   \item{site}{Character. Breast for breast cancer, All for all sites.}
#'   \item{period}{Character. Time period corresponding to data.}
#'   \item{sex}{Integer. Sex indicator (0 = Both, 1 = Male, 2 = Female).}
#'   \item{ages}{Integer. The starting age of the interval (e.g., 0, 1, 5...).}
#'   \item{cancer}{Integer. Count of first primary incident cancer cases.}
#'   \item{cancer_death}{Integer. Deaths specifically attributed to this cancer site.}
#'   \item{death}{Integer. Total deaths from all causes in the age interval.}
#'   \item{pys}{Numeric. Person-years at risk, typically the mid-year population.}
#' }
#'
#'
#' @source \doi{10.1002/sim.1428}
#'
#' @references
#' Fay MP, Pfeiffer R, Cronin KA, Le C, Feuer EJ. (2003).
#' Age-conditional probabilities of developing cancer.
#' \emph{Statistics in Medicine}, 22(11), 1837–1848.
#'
#' National Cancer Institute. DevCan 6.7.5: SEER 21 Incidence and Mortality,
#' 2019–2022 (2020 Excluded).
#'
#'
#' @examples
#' data(seer_example_data)
#' # View the structure of the Fay et al. example data
#' head(seer_example_data)
"seer_example_data"
