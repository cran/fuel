# FT-B
# 2020-06-18
# Test example (2020-06-18)
# LN1158(a=1,b=1,n=25,m=24, d=1/25, mean.rn=1, sd.rn=1)
#
LN1158 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
#
id <- 1158
  #
  mybeta <- sqrt(b + 3*a*a*d)
  mygamma <- sqrt(2*b + 4*a*a*d)
  myY <- sqrt(m*sd.rn*sd.rn)
  mynu <- 1+m/2*(b+ a*a*d)/(b - 3*a*a*d)
  #
  result<- exp(a*mean.rn) * (mybeta/mygamma)^mynu * besselK(x=mybeta*myY, nu=mynu) / besselK(x=mygamma*myY, nu=mynu)
  #
  return(result)
} #
