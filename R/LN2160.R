# ZG-17
# 
# 
LN2160 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
  #
  id <- 2160
  #
  #
  psi = 1 - 2/m - 3*a*a*d/b - 0.5*b/m*sd.rn^2
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  #
  return(result)
} #