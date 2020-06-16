# R-LO
#
#
LN1090 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
#
id <- 1090
  #
  tt <- (b - a^2*d)*sd.rn^2/2 # R-LO
  ww <- m/2
  mainpart <- PsiR(t=tt,w=ww,tol = 1e-9, maxItem=1e2)
  result <- exp(a*mean.rn)*mainpart[1]
  #
  return(result)
}
