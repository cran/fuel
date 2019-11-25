# ZG-10
#
LN2100 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
  id <- 2100
  #
  #
  psi = 1 - 5*a*a*d/b - 2/m
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  #
  return(result)
} #