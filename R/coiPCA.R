coiPCA <- function(Xa, Xb, row.w = NULL, ncp = 5) {
  
  if(is.null(row.w)) row.w <- rep(1, nrow(Xa))
  if(any(sapply(Xa, FUN = function(x) !is.numeric(x) & !is.integer(x)))) stop("variables in Xa should all be numeric")
  if(any(sapply(Xb, FUN = function(x) !is.numeric(x) & !is.integer(x)))) stop("variables in Xb should all be numeric")
  if(nrow(Xa) != nrow(Xb)) stop("Xa and Xb should have the same number of rows")
  
  Xas <- as.matrix(data.frame(lapply(Xa, function(x) (x-weighted.mean(x,row.w))/descriptio::weighted.sd(x,row.w))))
  Xbs <- as.matrix(data.frame(lapply(Xb, function(x) (x-weighted.mean(x,row.w))/descriptio::weighted.sd(x,row.w))))
  
  tabco <- t(Xas) %*% diag(row.w) %*% Xbs
  
  res <- gPCA(tabco, center = FALSE, scale = FALSE) #, ncp = ncp, graph = FALSE)
  class(res) <- list("PCA", "coiPCA", "list")
  
  res$RV <- rvcoef(Xa, Xb, row.w = row.w)
  
  return(res)
}
