# ZG-15
#
LN2150 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
  id <- 2150
  #
  #
  psi = 1 - 3*a*a*d/b - 0.5*b/m*sd.rn^2
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  #
  return(result)
} #