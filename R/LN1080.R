# R-F
# 
#
LN1080 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
#
id <- 1080
#
  tt <- m/(m+2)*(b - 3*a^2*d)*sd.rn^2/2 # R-F
  ww <- m/2
  PsiF <- gamma(ww)*(ww*tt)^((1-ww)/2)*besselI(x=2*sqrt(ww*tt), nu=ww-1)
  result <- exp(a*mean.rn)*PsiF
  #
  return(result)
}