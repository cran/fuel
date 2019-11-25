# R-LO
# 
### Function PsiR
PsiR <- function(t,w,tol = 1e-9, maxItem=1e2) {
  value <- 0 
  convergence <- 0
  for (k in 0:maxItem) {
    value.returned <- value
    item <- gamma(w+k)/gamma(w+k+k)*(w*t)^k/gamma(k+1)
    value <- value + item
    if (is.infinite(value)) {
      convergence <- 0
      break
    }
    if (abs(item/value) < tol) {
      value.returned <- value
      convergence <- 1
      break
    }
  }
  return(c(value.returned, k, convergence))
}
###
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