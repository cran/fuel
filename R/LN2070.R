# ZG-7
# 
# 
#
LN2070 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
  id <- 2070
  #
  #
  partA <- 3*a^4*d^2*m^2 + 2/3*b^3*sd.rn^2 + 3*a^2*d*m*b^2*sd.rn^2 + 3/4*b^4*sd.rn^4 
  partB <- m*(a^2*d*m*b + b^3*sd.rn^2/2)
  psi = 1 - partA/partB
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  #
  return(result)
} #
