# QML
# 
LN1010 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
id <- 1010
#
psi = 1
#
result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
#
return (result)
}
