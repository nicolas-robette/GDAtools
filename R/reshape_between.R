reshape_between <- function(bcmca) {
  
  bcmca$mycall$X <- as.data.frame(bcmca$mycall$X)
  
  bcmca$var <- bcmca$col
  colnames(bcmca$var$coord) <- gsub("Dim ","dim.", colnames(bcmca$var$coord))
  colnames(bcmca$var$contrib) <- gsub("Dim ","dim.", colnames(bcmca$var$contrib))
  colnames(bcmca$var$cos2) <- gsub("Dim ","dim.", colnames(bcmca$var$cos2))
  
  bcmca$var$v.contrib <- aggregate(bcmca$var$contrib, list(sub("\\..*", "", rownames(bcmca$var$contrib))), sum)[,-1]
  rownames(bcmca$var$v.contrib) <- unique(sub("\\..*", "", rownames(bcmca$var$contrib)))
  colnames(bcmca$var$v.contrib) <- paste("dim", 1:ncol(bcmca$var$v.contrib),".")
  
  disj <- dichotom(bcmca$mycall$X, out = "numeric")
  if(!is.null(bcmca$mycall$excl)) disj <- disj[,-bcmca$mycall$excl]
  eff <- t(as.matrix(disj)) %*% bcmca$mycall$row.w
  bcmca$var$weight <- as.numeric(eff)
  names(bcmca$var$weight) <- rownames(eff)

  eta2 = matrix(nrow=ncol(bcmca$mycall$X), ncol=bcmca$call$ncp)
  for(i in 1:nrow(eta2)) {
    for(j in 1:ncol(eta2))
      eta2[i,j] = summary(stats::lm(bcmca$row.sup$coord[,j]~bcmca$mycall$X[,i],weights=bcmca$mycall$row.w))$r.squared
  }
  rownames(eta2) = colnames(bcmca$mycall$X)
  colnames(eta2) = paste("dim", 1:ncol(eta2), sep=".")
  bcmca$var$eta2 = eta2
  dimnames(bcmca$var$v.contrib) = dimnames(eta2)
    
  bcmca$oldcall <- bcmca$call
  
  if(is.character(bcmca$mycall$excl)) {
    excl <- which(getindexcat(bcmca$mycall$X) %in% bcmca$mycall$excl)
  } else {
    excl <- bcmca$mycall$excl
  }

  bcmca$call <- list(
    X = bcmca$mycall$X,
    excl = excl,
    excl.char = getindexcat(bcmca$mycall$X)[excl],
    row.w = bcmca$mycall$row.w,
    ncp = bcmca$oldcall$ncp
  )
  
  bcmca$ind <- bcmca$row.sup
  colnames(bcmca$ind$coord) <- gsub("Dim ","dim.", colnames(bcmca$ind$coord))
  colnames(bcmca$ind$cos2) <- gsub("Dim ","dim.", colnames(bcmca$ind$cos2))
  
  oldeig <- bcmca$eig
  bcmca$eig <- list()
  bcmca$eig$eigen <- oldeig[,1]
  bcmca$eig$rate <- oldeig[,2]
  bcmca$eig$cum.rate <- oldeig[,3]
  bcmca$eig$mrate <- oldeig[,2]
  bcmca$eig$cum.mrate <- oldeig[,3]
  
  Z <- dichotom(bcmca$call$X)
  fK <- colSums(bcmca$call$row.w * Z) / sum(bcmca$call$row.w)
  if(!is.null(bcmca$mycall$excl)) fK <- fK[-bcmca$mycall$excl]
  bcmca$var$ctr.cloud <- data.frame(ctr.cloud = 100 * (1-fK) / (length(fK)-ncol(bcmca$call$X)))
  
  attr(bcmca,'class')[1] <- "bcMCA"
  attr(bcmca,'class')[2] <- "CA"

  return(bcmca)
}
