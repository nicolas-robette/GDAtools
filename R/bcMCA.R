bcMCA <- function(data, class, excl = NULL, row.w = NULL) {
  
  if(is.null(row.w)) row.w <- rep(1, nrow(data))
  if(any(sapply(data, FUN = function(x) !is.factor(x)))) stop("variables in data should all be factors")
  if(!is.factor(class)) stop("class should be a factor")
  if(is.character(excl)) excl <- which(getindexcat(data) %in% excl)

  n <- sum(row.w)
  nk <- tapply(row.w, class, sum)
  pk <- as.vector(nk / n)
  disj <- dichotom(data, out = "numeric")
  if(!is.null(excl)) disj <- disj[,-excl]
  centers <- sweep(t(as.matrix(dichotom(class)))%*%diag(row.w)%*%as.matrix(disj), 1, nk, "/")
  centers <- data.frame(centers)
  row.names(centers) <- levels(class)
  names(centers) <- names(disj)
  df <- rbind.data.frame(centers, disj)
  
  res <- FactoMineR::CA(df, 
                        row.w = as.vector(pk), 
                        row.sup = (nlevels(class)+1):nrow(df), 
                        ncp = nrow(centers)-1, 
                        graph = FALSE)
  
  mca <- FactoMineR::CA(dichotom(data, out = "numeric"),
                        row.w = row.w,
                        ncp = nrow(centers)-1,
                        graph = FALSE,
                        excl = excl)
  
  res$ratio <- sum(res$eig[,"eigenvalue"]) / sum(mca$eig[,"eigenvalue"])
  
  res$mycall <- list(X = data,
                     excl = excl,
                     class = class,
                     row.w = row.w,
                     ncp = nrow(centers)-1
                     )
  
  class(res) <- c("CA", "bcMCA", "list")
  return(res)
}

