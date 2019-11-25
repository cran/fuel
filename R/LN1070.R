# R-S
#
# LN1070(a=1,b=1,n=10,m=n-1,d=1/n,mean.rn=1,sd.rn=1)
# 
LN1070 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
id <- 1070
  #
  psi = m/(m+2)*(1 - 3*a^2*d/b)
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  #
  return(result)
}