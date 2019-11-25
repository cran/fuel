# ZG-5
# 
# 
#
LN2050 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
  #
  id <- 2050
  #
  #
  psi = 1 - 3*a*a*d/b - 4/m - 1.5*b*sd.rn*sd.rn/m
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  return(result)
} #