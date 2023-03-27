bootvalid_supvars <- function(resmca, vars = NULL, axes = c(1,2), K = 30) {
  
  if(is.null(vars)) stop("You should provide supplementary variables.")
  
  # donnees initiales
  X <- vars
  vs <- resmca$svd$vs[axes]

  bootco <- list()
  for(i in 1:K) {
    samp <- sample(1:nrow(X), nrow(X), replace = TRUE)
    Xboot <- X[samp,]
    iboot <- resmca$ind$coord[samp, axes]
    tdc <- dichotom(Xboot)
    n <- nrow(Xboot)
    FK <- colSums(tdc)/n
    temp <- (t(tdc) %*% iboot)
    temp <- apply(temp, 2, function(x) x/n/FK)
    temp <- t(apply(temp, 1, function(x) x/vs))
    bootco[[i]] <- data.frame(varcat = rownames(temp), K = rep(i, nrow(temp)), temp)
  }

  # bind results
  bootco <- do.call("rbind.data.frame", bootco)
  bootco$varcat <- factor(bootco$var)
  rownames(bootco) <- NULL
  bootco <- bootco[order(bootco$varcat),]
  
  return(bootco)
}
