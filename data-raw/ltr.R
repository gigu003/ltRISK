#' Estimate the age conditional probability of developing or dying from cancer
#'
#' @rdname ltr
#' @param x data
#' @param mi The annual number of all-cause mortality deaths in each age group.
#' @param di The annual number of cancer-related deaths in each age group.
#' @param ri The annual number of diagnosed cancer cases in each age group.
#' @param ni The number of population in each age group.
#' @param vars A character vector contains the names of variable in data.
#' @param age_width The age width of each age group.
#' @param type Characters "developing" or "dying" indicate estimate the
#'              probability of developing cancer or dying from it.
#'
#' @return A list with class of ltr:
#'   \item{age}{Age groups.}
#'   \item{si}{The probability of developing or dying from cancer
#'              in each age group.}
#'   \item{vari}{The variance of the estimates for each age group.}
#' @export
#'
ltr <- function(
  x,
  di,
  ri,
  ni,
  vars = c("mi", "di", "ri", "ni"),
  age_width = 5,
  type = "developing"
) {
  UseMethod("ltr", x)
}

#' @rdname ltr
#' @method ltr data.frame
#' @export
ltr.data.frame <- function(
  x,
  di = NULL,
  ri = NULL,
  ni = NULL,
  vars = c("mi", "di", "ri", "ni"),
  age_width = 5,
  type = "developing"
) {
  if (!all(vars %in% names(x))) {
    stop("Data frame must contain columns: 'mi', 'di', 'ri', 'ni'.")
  }
  mi <- x[[vars[1]]]
  di <- x[[vars[2]]]
  ri <- x[[vars[3]]]
  ni <- x[[vars[4]]]
  return(ltr_core(mi, di, ri, ni, age_width = age_width, type = type))
}


#' @rdname ltr
#' @method ltr default
#' @export
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
#'
ltr.default <- function(x, di, ri, ni, age_width = 5, type = "developing") {
  # Check if all vectors have the same length.
  if (!all(lengths(list(x, di, ri, ni)) == length(x))) {
    stop("All vectors (x, di, ri, ni) must have the same length.")
  }
  return(ltr_core(x, di, ri, ni, age_width = age_width, type = type))
}


ltr_core <- function(mi, di, ri, ni, age_width = 5, type = "developing") {
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
  lastagewidth <- 100 - (ll - 1) * age_width
  lastagewidth <- 5
  wi <- c(rep(age_width, ll - 1), lastagewidth)
  age <- seq(0, ll - 1) * age_width
  #sage_pos <- which(age == sage) + 1

  # Initialize survival probabilities
  s0ai <- rep(1, ll)
  poo <- s0ai
  for (i in 2:ll) {
    s0ai[i] <- s0ai[i - 1] *
      exp(-(mi[i - 1] - di[i - 1] + ri[i - 1]) * wi[i - 1] / ni[i - 1])
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
  var_s0 <- s0ai^2 * poo
  sx <- exp(-(wi / ni) * (ri + mi - di))
  esx <- 1 - sx
  pxi <- ri / (ri + mi - di)
  var_sx <- sx^2 * wi^2 * pxi * (1 - pxi) / ni
  vari <- (var_hc + ehc^2) *
    (var_s0 + es0^2) *
    (var_sx + esx^2) -
    ehc^2 * es0^2 * esx^2
  vari[is.na(vari)] <- 0
  si[is.na(si)] <- 0
  res <- list(age = age, si = si, vari = vari)
  class(res) <- "ltr"
  return(res)
}
