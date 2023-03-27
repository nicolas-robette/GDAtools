PCAiv <- function(Y, X, row.w = NULL, ncp = 5) {
  if(is.null(row.w)) row.w <- rep(1, nrow(Y))
  if(any(sapply(Y, FUN = function(x) !is.numeric(x) & !is.integer(x)))) stop("variables in Y should all be numeric")
  if(any(sapply(X, FUN = function(x) !is.numeric(x) & !is.integer(x) & !is.factor(x)))) stop("variables in X should all be factor or numeric")
  if(nrow(Y) != nrow(X)) stop("Y and X should have the same number of rows")
  Ys <- data.frame(lapply(Y, function(x) (x - stats::weighted.mean(x,row.w)) / descriptio::weighted.sd(x,row.w)))
  names(Ys) <- names(Y)
  for(i in 1:ncol(X)) {
    if(!is.factor(X[,i])) X[,i] <- (X[,i] - stats::weighted.mean(X[,i],row.w)) / descriptio::weighted.sd(X[,i],row.w)
  }
  lmiv <- function(y) {
    df <- data.frame(y = y, X)
    yhat <- stats::predict(stats::lm(y ~ . , data = df, weights = row.w))
    return(yhat)
  }
  YHAT <- do.call("cbind.data.frame", lapply(Ys, lmiv))
  df <- cbind.data.frame(YHAT, X)
  qualsup <- which(sapply(X, is.factor))
  if(length(qualsup)==0) qualsup <- NULL
  quantsup <- which(sapply(X, function(x) is.numeric(x) | is.integer(x)))
  if(length(quantsup)==0) quantsup <- NULL
  res <- FactoMineR::PCA(df, scale.unit = FALSE, ncp = ncp, row.w = row.w, quali.sup = (ncol(Y)+qualsup), quanti.sup = (ncol(Y)+quantsup), graph = FALSE)
  pca <- FactoMineR::PCA(Ys, scale.unit = FALSE, ncp = ncp, row.w = row.w, graph = FALSE)
  res$ratio <- sum(res$eig[,"eigenvalue"]) / sum(pca$eig[,"eigenvalue"])
  class(res) <- c("PCA", "PCAiv", "list")
  return(res)
}
