# F
# 
LN1041 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
#
id <- 1041
  #
  result <- exp(a*mean.rn)*gamma(m/2)*(m*(b-a^2*d)*sd.rn^2/4)^((1-m/2)/2)*besselI(x=sqrt(m*sd.rn^2*(b-a^2*d)), nu=m/2-1)
  #result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
return(result)
}