# library(explor)
# data(decathlon)
# Y = decathlon[,1:10]
# X = decathlon[,12:13]
# row.w = NULL
# ncp = 5

# acpvio <- PCAoiv(Y, X)
# explor(acpvio)


PCAoiv <- function(X, Z, row.w = NULL, ncp = 5) {
  if(is.null(row.w)) row.w <- rep(1, nrow(X))
  if(any(sapply(X, FUN = function(Z) !is.numeric(Z) & !is.integer(Z)))) stop("variables in X should all be numeric")
  if(any(sapply(Z, FUN = function(Z) !is.numeric(Z) & !is.integer(Z) & !is.factor(Z)))) stop("variables in Z should all be factor or numeric")
  if(nrow(X) != nrow(Z)) stop("X and Z should have the same number of rows")
  Xs <- data.frame(lapply(X, function(x) (x-weighted.mean(x,row.w))/weighted.sd(x,row.w)))
  names(Xs) <- names(X)
  for(i in 1:ncol(Z)) {
    if(!is.factor(Z[,i])) Z[,i] <- (Z[,i]-weighted.mean(Z[,i],row.w)) / weighted.sd(Z[,i],row.w)
  }
  lmoiv <- function(x) {
    df <- data.frame(x = x, Z)
    Xhat <- residuals(lm(x ~ . , data = df, weights = row.w))
    return(Xhat)
  }
  XHAT <- do.call("cbind.data.frame", lapply(Xs, lmoiv))
  df <- cbind.data.frame(XHAT, Z)
  qualsup <- which(sapply(Z, is.factor))
  if(length(qualsup)==0) qualsup <- NULL
  quantsup <- which(sapply(Z, function(Z) is.numeric(Z) | is.integer(Z)))
  if(length(quantsup)==0) quantsup <- NULL
  res <- FactoMineR::PCA(df, scale.unit = FALSE, ncp = ncp, row.w = row.w, quali.sup = (ncol(X)+qualsup), quanti.sup = (ncol(X)+quantsup), graph = FALSE)
  pca <- FactoMineR::PCA(Xs, scale.unit = FALSE, ncp = ncp, row.w = row.w, graph = FALSE)
  res$ratio <- sum(res$eig[,"eigenvalue"]) / sum(pca$eig[,"eigenvalue"])
  class(res) <- c("PCA", "PCAoiv", "list")
  return(res)
}

