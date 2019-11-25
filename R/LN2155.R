# ZG-16
# 
# 
#
LN2155 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
  #
  id <- 2155
  #
  #
  psi = 1 - 3*a*a*d/b - 2/m
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  #
  return(result)
} #