# ZG-4
# 
#
LN2040 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
  id <- 2040
  #
  psi = 1 - 3*a^2*d/b - 2*(1 - 3*a^2*d/b)/m - 3*b*(1 - 3*a^2*d/b)^2*sd.rn^2/(2*m)
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  return(result)
} #