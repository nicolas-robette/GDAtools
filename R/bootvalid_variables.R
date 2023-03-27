bootvalid_variables <- function(resmca, axes = c(1,2), type = "partial", K = 30) {
  
  # donnees initiales
  X <- resmca$call$X
  excl <- resmca$call$excl
  vs <- resmca$svd$vs[axes]
  coord <- data.frame(resmca$var$coord[,axes])
  names(coord) <- c("x", "y")

  if(type=="partial") {
    bootco <- list()
    for(i in 1:K) {
      samp <- sample(1:nrow(X), nrow(X), replace = TRUE)
      Xboot <- X[samp,]
      iboot <- supind(resmca, Xboot)$coord[,axes]
      tdc <- dichotom(Xboot)[,-excl]
      n <- nrow(Xboot)
      FK <- colSums(tdc)/n
      temp <- (t(tdc) %*% iboot)
      temp <- apply(temp, 2, function(x) x/n/FK)
      temp <- t(apply(temp, 1, function(x) x/vs))
      bootco[[i]] <- data.frame(varcat = rownames(temp), K = rep(i, nrow(temp)), temp)
    }

  # total bootstrap
  } else if(type %in% c("total1", "total2", "total3")) {
      bootco <- list()
      for(i in 1:K) {
        samp <- sample(1:nrow(X), nrow(X), replace = TRUE)
        Xboot <- X[samp,]
        mca <- speMCA(Xboot, excl = excl)
        temp <- data.frame(mca$var$coord[,axes])
        names(temp) <- c("x", "y")
        
        if(type %in% c("total1", "total2")) {
            # interversion of axes
            if(type=="total2") {
              if(abs(cor(temp$x, coord$y)) > abs(cor(temp$x, coord$x)) & 
                 abs(cor(temp$y, coord$x)) > abs(cor(temp$y, coord$y))) {
                temp <- temp[, c(2,1)]
                names(temp) <- c("x", "y")
              }
            }
            # change sign of axes
            temp$x <- temp$x * sign(cor(temp$x, coord$x))
            temp$y <- temp$y * sign(cor(temp$y, coord$y))
        }
        
        if(type=="total3") {
           temp <- procu(coord, temp)
          }
        
        # add results
        bootco[[i]] <- data.frame(varcat = rownames(temp), K = rep(i, nrow(temp)), temp)
      }
  }
  
  # bind results
  bootco <- do.call("rbind.data.frame", bootco)
  bootco$varcat <- factor(bootco$varcat)
  rownames(bootco) <- NULL
  bootco <- bootco[order(bootco$varcat),]
  
  return(bootco)
}
