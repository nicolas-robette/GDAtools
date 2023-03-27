coiMCA <- function(Xa, Xb, excl.a = NULL, excl.b = NULL, row.w = NULL, ncp = 5) {
  
  if(is.null(row.w)) row.w <- rep(1, nrow(Xa))
  if(any(sapply(Xa, FUN = function(x) !is.factor(x)))) stop("variables in Xa should all be factors")
  if(any(sapply(Xb, FUN = function(x) !is.factor(x)))) stop("variables in Xb should all be factors")
  if(nrow(Xa) != nrow(Xb)) stop("Xa and Xb should have the same number of rows")
  if(is.character(excl.a)) excl.a <- which(getindexcat(Xa) %in% excl.a)
  if(is.null(excl.a)) excl.a <- 99999
  if(is.character(excl.b)) excl.b <- which(getindexcat(Xb) %in% excl.b)
  if(is.null(excl.b)) excl.b <- 99999
  
  Xad <- as.matrix(dichotom(Xa, out = "numeric"))[,-excl.a]
  Xbd <- as.matrix(dichotom(Xb, out = "numeric"))[,-excl.b]
  tabco <- t(Xad) %*% diag(row.w) %*% Xbd
  
  res <- CA(tabco, ncp = ncp, graph = FALSE)
  class(res) <- list("CA", "coiMCA", "list")
  
  res$RV <- rvcoef(data.frame(Xad), data.frame(Xbd), row.w = row.w)

  return(res)
  }
