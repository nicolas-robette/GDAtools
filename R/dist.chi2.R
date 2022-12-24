dist.chi2 <- function(X) {
  if(any(sapply(X, FUN = function(x) !is.factor(x)))) stop("variables in X should all be factors")
  X <- dichotom(X, out = "numeric")
  xip = rowSums(X)
  R <- X / xip
  xpp = sum(xip)             # grand total
  m <- xip / xpp             # masses 
  c <- colSums(X) / xpp      # row barycenter
  w = 1/c                    # columns weights
  Rc = t(t(R) - c)           # deviations to barycenter
  Rtilde = t(t(Rc)*sqrt(w))  # weighted R
  S = Rtilde%*%t(Rtilde)     # covariance
  s = diag(S)                # diag of S
  D = (s - S) + t(s-S)       # Chi2 distance matrix
  return(D)
}
