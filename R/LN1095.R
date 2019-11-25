# R-B
# 
#
LN1095 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
#
id <- 1095
  #
  mybeta <- 0.5*sqrt((b - 3*a*a*d)/2)
  mygamma <- 1.5*sqrt((b - 3*a*a*d)/2)
  myY <- sqrt(m*sd.rn*sd.rn)
  result<- exp(a*mean.rn) *(1/3)^(m/2+2)*besselK(x=mybeta*myY, nu=m/2+2)/besselK(x=mygamma*myY, nu=m/2+2)
  #
  return(result)
} #