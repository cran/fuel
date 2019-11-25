# GT-ES
# 
# 
#
LN1160 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
  #
  id <- 1160
  #
  #
  part <- 1 - exp(-(b-3*a*a*d)*sd.rn*sd.rn/m)
  psi = m/b/sd.rn/sd.rn*part
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  return(result)
} #