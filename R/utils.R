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


agg.wtd.mean <- function(x, by, w) {
  res <- split(data.frame(w,x), by)
  if(ncol(res[[1]])==2) {
    res <- data.frame(mean = sapply(res, function(z) weighted.mean(z[,2], z[,1])))
  } else {
    res <- lapply(res, function(z) sapply(z[,-1], weighted.mean, w = z[,1])) |>
      do.call("rbind.data.frame", args = _)
    colnames(res) <- colnames(x)
    rownames(res) <- levels(by)
  }
  return(res)
}


agg.wtd.var <- function(x, by, w) {
  res <- split(data.frame(w,x), by)
  if(ncol(res[[1]])==2) {
    res <- data.frame(mean = sapply(res, function(z) descriptio::weighted.sd(z[,2], z[,1])))
  } else {
    res <- lapply(res, function(z) sapply(z[,-1], descriptio::weighted.sd, weights = z[,1])) |>
      do.call("rbind.data.frame", args = _)
    rownames(res) <- levels(by)
    colnames(res) <- colnames(x)
  }
  res <- res * res
  return(res)
}
