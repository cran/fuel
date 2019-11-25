# L-MS
# 
#
LN1150 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
  #
  id <- 1150
  #
  #
  part <- 1/(2 - exp(-(b-3*a*a*d)*sd.rn*sd.rn/(m+2)))
  psi = m/b/sd.rn/sd.rn*(1 - part)
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  return(result)
} #