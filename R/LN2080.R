# ZG-8
#
LN2080 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
  #
  id <- 2080
  #
  #
  psi = 1 - 6*a*a*d/b - 4/3/m - 1.5*b*sd.rn*sd.rn/m
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  #
  return(result)
} #