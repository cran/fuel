# SA
# 
#
LN1030 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
id <- 1030
#
  #
  psi = 1 - a^2*d/b
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
return (result)
}