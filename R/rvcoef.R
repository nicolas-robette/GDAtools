rvcoef <- function(Xa, Xb, row.w = NULL) {
  
  if(is.null(row.w)) row.w <- rep(1, nrow(Xa))
  if(any(sapply(Xa, FUN = function(x) !is.numeric(x) & !is.integer(x)))) stop("variables in Xa should all be numeric")
  if(any(sapply(Xb, FUN = function(x) !is.numeric(x) & !is.integer(x)))) stop("variables in Xb should all be numeric")
  if(nrow(Xa) != nrow(Xb)) stop("Xa and Xb should have the same number of rows")
  
  if(is.matrix(Xa)) Xa <- data.frame(Xa)
  if(is.matrix(Xb)) Xb <- data.frame(Xb)
  
  Xas <- as.matrix(data.frame(lapply(Xa, function(x) x-weighted.mean(x,row.w)))) 
  Xbs <- as.matrix(data.frame(lapply(Xb, function(x) x-weighted.mean(x,row.w)))) 
  
  tr <- function(Z) {sum(diag(Z))}
  W1 <- t(Xas) %*% diag(row.w) %*% Xas
  W2 <- t(Xbs) %*% diag(row.w) %*% Xbs
  W3 <- t(Xas) %*% diag(row.w) %*% Xbs
  W4 <- t(Xbs) %*% diag(row.w) %*% Xas
  rv <- tr(W3 %*% W4) / sqrt(tr(W1 %*% W1) * tr(W2 %*% W2))
  
  return(rv)
}
