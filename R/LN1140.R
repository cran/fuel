# L-UB
# GT-F
# 
#
#
LN1140 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
  id <- 1140
  #
  #
  psi = m/b/sd.rn/sd.rn*(1 - exp(-(b-a*a*d)*sd.rn*sd.rn/m))
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  return(result)
} #