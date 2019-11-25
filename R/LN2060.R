# ZG-6
# 
# 
#
LN2060 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
  id <- 2060
  #
  #
  psi = 1 - 3*a*a*d/b - 4*(1 - 3*a*a*d/b)/m - 1.5*b*(1 - 3*a*a*d/b)^2*sd.rn*sd.rn/m
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  #
  return(result)
} #
