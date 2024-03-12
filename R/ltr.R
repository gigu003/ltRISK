#' Estimate the age conditional probability of developing or dying from cancer
#'
#' @param mi The annual number of all-cause mortality deaths in each age group.
#' @param di The annual number of cancer-related deaths in each age group.
#' @param ri The annual number of diagnosed cancer cases in each age group.
#' @param ni The number of population in each age group.
#' @param type Characters "developing" or "dying" indicate estimate the
#'              probability of developing cancer or dying from it.
#' @param age_width The age width of each age group.
#'
#' @return A list with class of ltr:
#'   \item{age}{Age groups.}
#'   \item{si}{The probability of developing or dying from cancer
#'              in each age group.}
#'   \item{vari}{The variance of the estimates for each age group.}
#' @export
#'
#'
#' @examples
#' ni <- c(
#'   73872987, 82029530, 72267070, 78303514, 99425613, 119915673, 98068725,
#'   96644427, 121225951, 121250720, 96012917, 79863455, 75972753, 52929797,
#'   37551107, 29047207, 19584254, 13854299
#' )
#' mi <- c(
#'   60594, 17718, 18883, 28127, 37493, 75223, 83574, 100655, 211467, 278913,
#'   419663, 445223, 770865, 929008, 1058922, 1346942, 1576852, 2305312
#' )
#' di <- c(
#'   3511, 2801, 2553, 3183, 4960, 9456, 13509, 23935, 62386, 111640, 147866,
#'   203955, 301892, 304985, 302785, 323804, 275557, 197614
#' )
#' ri <- c(
#'   9303, 6887, 6248, 8509, 16961, 39439, 56670, 86535, 189251, 289320, 344395,
#'   411232, 552071, 491213, 433786, 395544, 292672, 173503
#' )
#' ll <- ltr(mi, di, ri, ni)
ltr <- function(mi, di, ri, ni, type = "developing", age_width = 5) {
  # Check if the length of mi, di, ri, and ni equals.
  if (!all(lengths(list(mi, di, ri, ni)) == length(mi))) {
    stop("All vectors should have the same length.")
  }

  # Estimate the risk of developing cancer or dying from cancer.
  if (type == "dying") {
    ri <- di
  } else if (!type %in% c("developing", "dying")) {
    stop(paste("type", type, "was not supported."))
  }

  # calculate number of age groups
  ll <- length(mi)
  lastagewidth <- 100 - (ll - 1) * 5
  wi <- c(rep(age_width, ll - 1), lastagewidth)
  age <- seq(0, ll - 1) * age_width

  # calculate probability of being alive and cancer free at each age group.
  s0ai <- rep(1, ll)
  poo <- s0ai
  for (i in 2:ll) {
    s0ai[i] <- s0ai[i - 1] * exp(-(mi[i - 1] - di[i - 1] + ri[i - 1]) * wi[i - 1] / ni[i - 1])
    poj <- (ri[i - 1] + mi[i - 1] - di[i - 1]) / ni[i - 1]
    poo[i] <- poo[i - 1] * poj * (1 - poj) / ni[i - 1]
  }
  # probability of developing cancer at each age group
  si <- ri / (ri + mi - di) * s0ai * (1 - exp(-(wi / ni) * (ri + mi - di)))
  si <- ifelse(is.na(si), 0, si)
  si[ll] <- ri[ll] / (ri[ll] + mi[ll] - di[ll]) * s0ai[ll]

  # calculate variance of s using method of binomial
  pci <- ri / (ri + mi - di)
  var_hc <- pci * (1 - pci) / (ri + mi - di)
  ehc <- pci
  es0 <- s0ai
  poj <-
    var_s0 <- s0ai^2 * poo

  sx <- exp(-(wi / ni) * (ri + mi - di))
  esx <- 1 - sx
  pxi <- ri / (ri + mi - di)
  var_sx <- sx^2 * wi^2 * pxi * (1 - pxi) / ni
  vari <- (var_hc + ehc^2) * (var_s0 + es0^2) * (var_sx + esx^2) - ehc^2 * es0^2 * esx^2
  res <- list(age = age, si = si, vari = vari)
  attr(res, "class") <- c("ltr", "list")
  return(res)
}


#' Estimate the lifetime risk and 95% confidence interval
#'
#' @param x Object of class 'ltr' outputed by ltr function.
#' @param sage The initial age of the lifetime risk.
#'
#' @return Life time risk and 95% CI.
#' @export
#'
estimate.ltr <- function(x, sage = 0, mp = 100) {
  pos <- which(x$age == sage)
  ll <- length(x$age)
  p <- sum(x$si[pos:ll])
  var <- sum(x$vari[pos:ll])
  lower <- (p - 1.96 * sqrt(var))*mp
  upper <- (p + 1.96 * sqrt(var))*mp
  p <- p*mp
  res <- c(p, lower, upper)
  names(res) <- c("Risk", "Lower_CI", "Upper_CI")
  return(res)
}

#' Test the difference of lifetime risk between two groups using Z test
#'
#' @param x Age conditional probability in group x with class of 'ltr'
#'          returned by function of ltr.
#' @param y Age conditional probability in group y with class of 'ltr'
#'          returned by the ltr function.
#' @return The statistics Z and P of the test result.
#' @export
#'
ztest.ltr <- function(x, y) {
  z <- abs(sum(x$si) - sum(y$si)) / sqrt(sum(x$vari) + sum(y$vari))
  p <- ifelse(z > 2.56, "< 0.01",
    ifelse(z > 1.96, "< 0.05", "> 0.05")
  )
  return(list(z = z, p = p))
}
