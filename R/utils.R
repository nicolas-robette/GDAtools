getvarnames <- function(resmca) {
  X <- resmca$call$X
  var <- unlist(lapply(names(X), function(x) rep(x, nlevels(X[[x]]))))
  cat <- unlist(lapply(X, levels))
  varcat <- paste(var, cat, sep = ".")
  res <- data.frame(var, cat, varcat)
  rownames(res) <- NULL
  return(res)
}


# cf vegan::procrustes()
# https://github.com/vegandevs/vegan/blob/master/R/procrustes.R
procu <- function(X, Y) {  
  ctrace <- function(MAT) sum(MAT^2)
  X <- scale(X, scale = FALSE)
  Y <- scale(Y, scale = FALSE)
  X <- X / sqrt(ctrace(X))
  Y <- Y / sqrt(ctrace(Y))
  XY <- crossprod(X, Y)
  sol <- svd(XY)
  A <- sol$v %*% t(sol$u)
  Yrot <- Y %*% A
  return(Yrot)
}
