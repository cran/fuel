# SZ-MM
# 
#
LN1120 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
  #
  id <- 1120
  #
  psi = m/(m+2 + 3*a*a*d*m/b + 1.5*b*sd.rn*sd.rn)
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  return(result)
} #