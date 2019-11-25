# SZ-MB
# 
#
LN1130 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
  id <- 1130
  #
  psi = m/(m + a*a*d*m/b + 0.5*b*sd.rn*sd.rn)
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  return(result)
} #