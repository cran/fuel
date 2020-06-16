# Zh
# Modified on 2020-06-16
#
LN1110 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
#
id <- 1110
  #
  tt <- (b - 4*a^2*d)*sd.rn^2/2 # Zh
  ww <- m/2
  if (b - 4*a^2*d > 0) {
    PsiF <- gamma(ww)*(ww*tt)^((1-ww)/2)*besselI(x=2*sqrt(ww*tt), nu=ww-1)
  } else {
    # For large d, high leverage (2020-06-16)
    # Followed the way we edit LN1041
    mainpart <- PsiF(t=tt, w=ww, tol=1e-9, maxItem=1e2)
    PsiF <- mainpart[1]
  }
  result <- exp(a*mean.rn)*PsiF
  #result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  #
  return(result)
} #
