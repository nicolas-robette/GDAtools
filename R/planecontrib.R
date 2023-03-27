planecontrib <- function(resmca, axes = c(1,2)) {
  
  # contributions and cosines of categories
  n <- nrow(resmca$ind$coord)
  Q <- nrow(resmca$var$v.contrib)
  fk <- resmca$var$weight / n
  yk2 <- rowSums(resmca$var$coord[,axes] * resmca$var$coord[,axes])
  ltot <- sum(resmca$eig$eigen[axes])
  ctr12 <- fk*yk2/(Q*ltot)
  ctr12 <- ctr12 * 100 / sum(ctr12)
  cos12 <- rowSums(resmca$var$cos2[,axes])

  # contributions of individuals
  ind <- list()
  fi <- resmca$call$row.w / sum(resmca$call$row.w)
  yi2 <- rowSums(resmca$ind$coord[,axes] * resmca$ind$coord[,axes])
  ind$ctr12 <- fi*yi2/ltot
  ind$ctr12 <- ind$ctr12 * 100 / sum(ind$ctr12)
  ind$cos12 <- rowSums(resmca$ind$cos2[,axes])

  # contributions of variables
  s <- vector()
  for (i in 1:Q) s <- c(s, rep(i, times = length(levels(resmca$call$X[,i]))))
  if(!is.null(resmca$call$excl)) s <- s[-resmca$call$excl]
  vctr12 <- aggregate(ctr12, list(s), sum)[, -1]
  names(vctr12) <- colnames(resmca$call$X)
  
  # results
  var <- list(ctr12, cos12, vctr12)
  names(var) <- c(paste0("ctr", paste0(axes, collapse="")),
                  paste0("cos", paste0(axes, collapse="")),
                  paste0("vctr", paste0(axes, collapse="")))
  res <- list(var = var, ind = ind)
  return(res)
}
