# ZG-14
# 
# 
#
LN2140 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
  #
  id <- 2140
  #
  #
  psi = 1 - 10/3/m - 5*a*a*d/b - 2.5*b/m*sd.rn^2
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  #
  return(result)
} #