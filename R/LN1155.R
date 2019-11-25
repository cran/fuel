# FT
# 
#
LN1155 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
  id <- 1155
  #
  #
  psi = (1 - 3*a^2*d/b)/(1 + b*sd.rn^2/m)
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  return(result)
} #