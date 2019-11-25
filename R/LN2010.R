# ZG-1
# 
#
#
LN2010 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
  #
  id <- 2010
  #
  #
  psi = 1 - a^2*d/b - b*sd.rn^2/(2*m)
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  return(result)
} #