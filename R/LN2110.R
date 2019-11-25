# ZG-11
#
LN2110 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
  id <- 2110
  #
  #
  psi = 1 - 10*a*a*d/b - 10/3/m - 2.5*b/m*sd.rn*sd.rn
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  #
  return(result)
} #