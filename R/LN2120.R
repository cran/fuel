# ZG-12
#
LN2120 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
  id <- 2120
  #
  #
  lmd.n = 5/3*b*sd.rn^2 + 1.25*b*b*sd.rn^4 + 2*a^2*d*m/b + 5*a^2*d*m*sd.rn^2 + 5*a^4*d^2*m^2/(b^2)
  lmd.d = b*sd.rn^2/2 + a^2*d*m/b
  lmd = - lmd.n/lmd.d
  psi = 1 + lmd/m
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  #
  return(result)
} #