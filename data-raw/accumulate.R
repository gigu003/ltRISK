accumulate <- function(x) {
  ll <- length(x$age)
  si <- x$si
  vari <- x$vari
  ssi <- rep(0, ll)
  vvari <- rep(0, ll)
  for (i in 1:ll) {
    if (i == 1) {
      ssi[1] <- si[1]
      vvari[1] <- vari[1]
    } else {
      ssi[i] <- sum(si[1:i])
      vvari[i] <- sum(vari[1:i])
    }
  }
  res <- list()
  res$age <- x$age
  res$si <- ssi
  res$vari <- vvari
  class(res) <- "ltr"
  return(res)
}
