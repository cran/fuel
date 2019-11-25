# ZG-9
#
LN2090 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
  id <- 2090
  #
  #
  psi = 1 - 3*a*a*d/b - b*sd.rn*sd.rn/m *(1.5 + 2*b/(3*a*a*d*m))
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  #
  return(result)
} #