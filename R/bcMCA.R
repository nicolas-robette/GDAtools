bcMCA <- function(data, class, excl = NULL, row.w = NULL, ncp = 5) {
  
  if(is.null(row.w)) row.w <- rep(1, nrow(data))
  if(any(sapply(data, FUN = function(x) !is.factor(x)))) stop("variables in data should all be factors")
  if(!is.factor(class)) stop("class should be a factor")
  if(is.character(excl)) excl <- which(getindexcat(data) %in% excl)
  if(is.null(excl)) excl <- 99999
  
  n <- sum(row.w)
  nk <- tapply(row.w, class, sum)
  pk <- as.vector(nk / n)
  disj <- dichotom(data, out = "numeric")[,-excl]
  centers <- sweep(t(as.matrix(dichotom(class)))%*%diag(row.w)%*%as.matrix(disj), 1, nk, "/")
  centers <- data.frame(centers)
  row.names(centers) <- levels(class)
  names(centers) <- names(disj)
  df <- rbind.data.frame(centers, disj)
  
  res <- FactoMineR::CA(df, 
                        row.w = as.vector(pk), 
                        row.sup = (nlevels(class)+1):nrow(df), 
                        ncp = ncp, 
                        graph = FALSE)
  
  mca <- FactoMineR::CA(disj, row.w = row.w, ncp = ncp, graph = FALSE)
  res$ratio <- sum(res$eig[,"eigenvalue"]) / sum(mca$eig[,"eigenvalue"])
  
  class(res) <- c("CA", "bcMCA", "list")
  return(res)
}

