#' Calculate the cumulative rate.
#'
#' @description
#' It was used to calculate the cumulative rate up to a defined age limit.
#'
#' @param count Number of cases number.
#' @param pop Number of population at risk.
#' @param rate Age specific incidence or mortality rate.
#' @param eage The defined age limit of the calculated cumulative rate.
#' @param agewidth The age band width of the age specific rate.
#' @param sep_zero Logical value, if the 0 age group was separate.
#' @param mp A multiplier used to scale the calculated rates. Default is 100.
#' @param decimal Decimals of the calculated rates.
#'
#' @return Cumulative rate.
#' @export
#'
#' @examples
#' px <- c(20005, 86920, 102502, 151494, 182932, 203107, 240289, 247076, 199665,
#'         163820, 145382, 86789, 69368, 51207, 39112, 20509, 12301, 6586, 1909)
#' dx <- c(156, 58, 47, 49, 48, 68, 120, 162, 160, 294, 417, 522, 546, 628,
#'         891, 831, 926, 731, 269)
#' mx <- dx / px
#' cumrate(mx, eage = 70)
cumrate <- function(count,
                    pop,
                    rate = NULL,
                    eage = 70,
                    agewidth = 5,
                    sep_zero = TRUE,
                    mp = 1,
                    decimal = 5){
  if (missing(pop) && !missing(count) && is.null(rate)) {
    rate <- count
    count <- NULL
  }
  if (missing(count) == TRUE && !missing(pop) == TRUE && is.null(rate) ==
      TRUE) {
    count <- rate * pop
  }
  if (missing(pop) == TRUE && !missing(count) == TRUE && is.null(rate) ==
      TRUE) {
    pop <- count / rate
  }
  if (is.null(rate) == TRUE && !missing(count) == TRUE && !missing(pop) ==
      TRUE) {
    rate <- count / pop
  }
  nn <- length(rate)
  if (sep_zero){
    age <- c(0, 1, seq(agewidth, (nn-2)*agewidth, agewidth))
    agewidth <- c(1, agewidth-1, rep(agewidth, nn-2))
  } else {
    age <- c(0, seq(agewidth, (nn-1)*agewidth, agewidth))
    agewidth <- rep(agewidth, nn)
  }
  pos <- which(age == eage)
  cr <- sum(agewidth[1:pos] * rate[1:pos])
  cr <- round(cr*mp, decimal)
  names(cr) <- paste0("Cumulative Rate(1/", mp, ")")
  return(cr)
}

#' Calculate the cumulative risk.
#'
#' @description
#' It was used to calculate the cumulative risk based on cumulative rate.
#'
#' @param cumrate Cumulative rate.
#' @param mp A multiplier used to scale the calculated rates. Default is 100.
#' @param decimal Decimals of the calculated rates.
#'
#' @return Cumulative risk.
#' @export
#'
#' @examples
#' px <- c(20005, 86920, 102502, 151494, 182932, 203107, 240289, 247076, 199665,
#'         163820, 145382, 86789, 69368, 51207, 39112, 20509, 12301, 6586, 1909)
#' dx <- c(156, 58, 47, 49, 48, 68, 120, 162, 160, 294, 417, 522, 546, 628,
#'         891, 831, 926, 731, 269)
#' mx <- dx / px
#' cumrate(mx, eage = 70)
#' cumrisk(cumrate(mx, eage = 70))
cumrisk <- function(cumrate, mp = 100, decimal = 2){
  risk <- round((1 - exp(-cumrate)) * 100, decimal)
  names(risk) <- paste0("Cumulative Risk (1/", mp, ")")
  return(risk)
}
