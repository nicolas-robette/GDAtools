MCAiv <- function(Y, X, excl = NULL, row.w = NULL, ncp = 5) {
  if(is.null(row.w)) row.w <- rep(1, nrow(Y))
  if(any(sapply(Y, FUN = function(x) !is.factor(x)))) stop("variables in Y should all be factors")
  if(any(sapply(X, FUN = function(x) !is.numeric(x) & !is.integer(x) & !is.factor(x)))) stop("variables in X should all be factor or numeric")
  if(nrow(Y) != nrow(X)) stop("Y and X should have the same number of rows")
  for(i in 1:ncol(X)) {
    if(!is.factor(X[,i])) X[,i] <- (X[,i]-weighted.mean(X[,i],row.w)) / weighted.sd(X[,i],row.w)
  }
  mca0 <- speMCA(Y, excl = excl)
  ncomp <- sum(mca0$eig$eigen > 1e-10)
  F <- as.data.frame(speMCA(Y, excl = excl, ncp = ncomp)$ind$coord)
  lmiv <- function(y) {
    df <- data.frame(y = y, X)
    yhat <- stats::predict(stats::lm(y ~ . , data = df, weights = row.w))
    return(yhat)
  }
  FHAT <- do.call("cbind.data.frame", lapply(F, lmiv))
  df <- cbind.data.frame(FHAT, Y, X)
  qualsup <- which(sapply(df, is.factor))
  qualsup <- qualsup[qualsup > ncol(FHAT)]
  if(length(qualsup)==0) qualsup <- NULL
  quantsup <- which(sapply(df, function(x) is.numeric(x) | is.integer(x)))
  quantsup <- quantsup[quantsup > ncol(FHAT)]
  if(length(quantsup)==0) quantsup <- NULL
  res <- FactoMineR::PCA(df, scale.unit = FALSE, ncp = ncp, row.w = row.w, quali.sup = qualsup, quanti.sup = quantsup, graph = FALSE)
  pca <- FactoMineR::PCA(F, scale.unit = FALSE, ncp = ncp, row.w = row.w, graph = FALSE)
  res$ratio <- sum(res$eig[,"eigenvalue"]) / sum(pca$eig[,"eigenvalue"])
  class(res) <- c("PCA", "MCAiv", "list")
  return(res)
}
