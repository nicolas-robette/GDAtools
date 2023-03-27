bcPCA <- function(data, class, row.w = NULL, scale.unit = TRUE, ncp = 5) {
  if(is.null(row.w)) row.w <- rep(1, nrow(data))
  if(any(sapply(data, FUN = function(x) !is.numeric(x) & !is.integer(x)))) stop("variables in data should all be numeric or integer")
  if(!is.factor(class)) stop("class should be a factor")
  moy <- apply(data, 2, stats::weighted.mean, w = row.w)
  data <- sweep(data, 2, moy, FUN = "-")
  if(isTRUE(scale.unit)) {
    etype <- apply(data, 2, descriptio::weighted.sd, w = row.w)
    data <- sweep(data, 2, etype, FUN = "/")
  }
  n <- sum(row.w)
  nk <- tapply(row.w, class, sum)
  pk <- as.vector(nk / n)
  centers <- sweep(t(as.matrix(dichotom(class)))%*%diag(row.w)%*%as.matrix(data), 1, nk, "/")
  centers <- data.frame(centers)
  row.names(centers) <- levels(class)
  names(centers) <- names(data)
  df <- rbind.data.frame(centers, data)
  res <- PCA(df,
             scale.unit = FALSE, 
             row.w = pk,
             ind.sup = (nlevels(class)+1):nrow(df),
             ncp = ncp,
             graph = FALSE)
  res$call$scale.unit <- scale.unit
  pca <- FactoMineR::PCA(data, row.w = row.w, scale.unit = scale.unit, ncp = ncp, graph = FALSE)
  res$ratio <- sum(res$eig[,"eigenvalue"]) / sum(pca$eig[,"eigenvalue"])
  class(res) <- c("PCA", "bcPCA", "list")
  return(res)
}
