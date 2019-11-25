# ZG-13
#
LN2130 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
  id <- 2130
  #
  #
  psi = 1 - 2/m - 5*a*a*d/b - 2.5*b/m*sd.rn^2 - 2/3*b*b*sd.rn^2/(a^2*d*m^2)
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  #
  return(result)
} #