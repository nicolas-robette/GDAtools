# data(tea)
# res.mca <- MCA(tea, quanti.sup = 19, quali.sup = 20:36)
# Y <- tea[,1:18]
# X <- tea[,19:22]
# row.w = NULL
# ncp = 5
# excl = NULL

# mcavi <- MCAiv(Y, X)
# explor(mcavi)

# names(mcavi)[3:4] <- c("ind","var")
# class(mcavi)[1] <- "MCA"

MCAiv <- function(Y, X, excl = NULL, row.w = NULL, ncp = 5) {
  if(is.null(row.w)) row.w <- rep(1, nrow(Y))
  if(any(sapply(Y, FUN = function(x) !is.factor(x)))) stop("variables in Y should all be factors")
  if(any(sapply(X, FUN = function(x) !is.numeric(x) & !is.integer(x) & !is.factor(x)))) stop("variables in X should all be factor or numeric")
  if(nrow(Y) != nrow(X)) stop("Y and X should have the same number of rows")
  # Ydic <- dichotom(Y)
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
  class(res) <- c("PCA", "MCAiv", "list")
  return(res)
}
