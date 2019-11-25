# Z
# 
#
LN1050 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
id <- 1050
  #
  psi = 1 - 3*a^2*d/b
  #
  result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
return(result)
}