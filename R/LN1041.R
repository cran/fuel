# F
# Modified on 2020-06-16
# Test example (2020-06-16)
# LN1041(a=1,b=1,n=25,m=24, d=1/4, mean.rn=1, sd.rn=1)
#
#
LN1041 <- function (a,b,n,m=n-1,d=1/n,mean.rn,sd.rn) {
  #
  id <- 1041
  #
  if (a*a*d < b) { #
  result <- exp(a*mean.rn)*gamma(m/2)*(m*(b-a^2*d)*sd.rn^2/4)^((1-m/2)/2)*besselI(x=sqrt(m*sd.rn^2*(b-a^2*d)), nu=m/2-1)
  #result <- exp(a*mean.rn + b*psi*sd.rn^2/2)
  } else {
    # For large d, high leverage (2020-06-16)
    # Originally from LN1042. (2020-06-16)
    # LN1041 and 1042 are combined here. (2020-06-16)
    tt <- (b - a^2*d)*sd.rn^2/2 # F
    ww <- m/2
    mainpart <- PsiF(t=tt, w=ww, tol=1e-9, maxItem=1e2)
    result <- exp(a*mean.rn)*mainpart[1]
  }
  return(result)
}
