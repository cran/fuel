# EV
# 
#
LN1100 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
id <- 1100
  #
  psi = 1 - a^2*d/b - b*sd.rn^2/2/m - b*b*sd.rn^4/3/m/m
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  #
  return(result)
} #