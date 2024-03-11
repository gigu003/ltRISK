ltr <- function(mi, di, ri, ni, type = 1){
  if (!type == 1) ri <- di
  wi <- c(rep(5,17),15)
  #calculate number of age groups
  ll <- length(mi)
  #calculate probability of being alive and cancer free at each age group.
  s0ai <- rep(1, ll)
  poo <- s0ai
  for(i in 2:ll){
    s0ai[i] <- s0ai[i-1]*exp(-(mi[i-1]-di[i-1]+ri[i-1])*wi[i-1]/ni[i-1])
    poj <- (ri[i-1]+mi[i-1]-di[i-1])/ni[i-1]
    poo[i] <- poo[i-1]*poj*(1-poj)/ni[i-1]
  }
  #probability of developing cancer at each age group
  si <- ri/(ri+mi-di)*s0ai*(1-exp(-(wi/ni)*(ri+mi-di)))
  si <- ifelse(is.na(si),0,si)
  si[ll] <- ri[ll]/(ri[ll]+mi[ll]-di[ll])*s0ai[ll]
  s <- sum(si)

  #calculate variance of s using method of binomial
  pci <- ri/(ri+mi-di)
  var_hc <- pci*(1-pci)/(ri+mi-di)
  ehc <- pci
  es0 <- s0ai
  poj <-
    var_s0 <- s0ai^2*poo

  sx <- exp(-(wi / ni)*(ri + mi - di))
  esx <- 1 - sx
  pxi <- ri / (ri + mi - di)
  var_sx <- sx^2*wi^2*pxi*(1-pxi)/ni
  vari <-(var_hc+ehc^2)*(var_s0+es0^2)*(var_sx+esx^2)-ehc^2*es0^2*esx^2
  var <- sum(vari)
  lower <- s-1.96*sqrt(var)
  upper <- s+1.96*sqrt(var)
  #probability of dying
  p_m <- (2*5*mi/ni)/(2+5*mi/ni)
  #number still alive
  n_a <- rep(1, ll)
  for(i in 2:ll){
    n_a[i] <- n_a[i-1] - n_a[i-1]*p_m[i-1]
  }
  #person years
  p_y <- rep(1, ll)
  for(i in 1:(ll-1)){
    p_y[i] <- 5*(n_a[i]+n_a[i+1])/2
  }
  p_y[ll] <- n_a[ll]/(mi[ll]/ni[ll])
  agerate <- ri/ni
  #current probability of dying
  death <- p_y*agerate
  d_r <- sum(death)

  s40 <- sum(si[9:18])
  var40 <- sum(vari[9:18])
  s40lower <- s40-1.96*sqrt(var40)
  s40upper <- s40+1.96*sqrt(var40)

  s50 <- sum(si[11:18])
  var50 <- sum(vari[11:18])
  s50lower <- s50-1.96*sqrt(var50)
  s50upper <- s50+1.96*sqrt(var50)

  s60 <- sum(si[13:18])
  var60 <- sum(vari[13:18])
  s60lower <- s60-1.96*sqrt(var60)
  s60upper <- s60+1.96*sqrt(var60)

  s70 <- sum(si[15:18])
  var70 <- sum(vari[15:18])
  s70lower <- s70-1.96*sqrt(var70)
  s70upper <- s70+1.96*sqrt(var70)
  s80 <- sum(si[17:18])
  return(list(s = c(s, lower, upper),
              var=var,
              s40=c(s40, s40lower, s40upper),
              s50=c(s50, s50lower, s50upper),
              s60=c(s60, s60lower, s60upper),
              s70=c(s70, s70lower, s70upper),
              s80=s80))
}
