#' GCO_Today_2022
#'
#' @description A data frame containing the number of cancer cases, cancer
#' deaths (cancer mortality), all-cause deaths, and population counts stratified
#' by age groups, sex, and cancer sites in 20 regions worldwide and world total.
#' The cancer cases and cancer deaths are obtained from the Global
#' Cancer Observatory Today, while the population counts and all-cause deaths
#' are sourced from the World Population Prospects 2022.
#'
#' @format A data frame with 40,824 rows and 8 variables:
#' \describe{
#'   \item{site_code}{\code{integer}: Cancers include the code of cancer sites.}
#'   \item{icd10}{\code{character}: ICD-10 codes corresponding to cancer sites.}
#'   \item{site_abbr}{\code{character}: Cancer site abbreviated description.}
#'   \item{ages}{\code{integer}: Starting age of each age group.}
#'   \item{cancer}{\code{numeric}: Number of (registered) cancer cases.}
#'   \item{cancer_death}{\code{numeric}: Number of cancer deaths (cancer mortality).}
#'   \item{death}{\code{numeric}: Number of deaths (all-cause mortality).}
#'   \item{pys}{\code{numeric}: The size of the mid-year population.}
#' }
#' @details
#' The data is collected from two main sources: cancer cases and deaths from
#' the Global Cancer Observatory Today, and population and all-cause deaths from
#' the World Population Prospects 2022. The data covers 20 world regions and
#' includes estimates for different age groups and sexes.
#'
#' @source The cancer cases and cancer deaths are obtained from the Global
#'        Cancer Observatory Today \url{https://gco.iarc.fr/today/en}, while
#'        the population counts and all-cause deaths are sourced from the World
#'        Population Prospects 2022 \url{https://population.un.org/wpp/}.
#'
#' @references
#' Global Cancer Observatory Today: \url{https://gco.iarc.fr/today/en}
#' World Population Prospects 2022: \url{https://population.un.org/wpp/}
#'
#' @keywords GLOBOCAN GCO
#'
#' @examples
#' # Load the dataset
#' data(GCO_Today_2022)
#'
#' # Display the first few rows
#' head(GCO_Today_2022)
#'
"GCO_Today_2022"
