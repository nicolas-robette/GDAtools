reshape_between <- function(resmca) {
  
  resmca$mycall$X <- as.data.frame(resmca$mycall$X)
  
  resmca$var <- resmca$col
  colnames(resmca$var$coord) <- gsub("Dim ","dim.", colnames(resmca$var$coord))
  colnames(resmca$var$contrib) <- gsub("Dim ","dim.", colnames(resmca$var$contrib))
  colnames(resmca$var$cos2) <- gsub("Dim ","dim.", colnames(resmca$var$cos2))
  
  resmca$var$v.contrib <- aggregate(resmca$var$contrib, list(sub("\\..*", "", rownames(resmca$var$contrib))), sum)[,-1]
  rownames(resmca$var$v.contrib) <- unique(sub("\\..*", "", rownames(resmca$var$contrib)))
  colnames(resmca$var$v.contrib) <- paste("dim", 1:ncol(resmca$var$v.contrib),".")
  
  disj <- dichotom(resmca$mycall$X, out = "numeric")
  if(!is.null(resmca$mycall$excl)) disj <- disj[,-resmca$mycall$excl]
  eff <- t(as.matrix(disj)) %*% resmca$mycall$row.w
  resmca$var$weight <- as.numeric(eff)
  names(resmca$var$weight) <- rownames(eff)

  eta2 = matrix(nrow=ncol(resmca$mycall$X), ncol=resmca$call$ncp)
  for(i in 1:nrow(eta2)) {
    for(j in 1:ncol(eta2))
      eta2[i,j] = summary(stats::lm(resmca$row.sup$coord[,j]~resmca$mycall$X[,i],weights=resmca$mycall$row.w))$r.squared
  }
  rownames(eta2) = colnames(resmca$mycall$X)
  colnames(eta2) = paste("dim", 1:ncol(eta2), sep=".")
  resmca$var$eta2 = eta2
  dimnames(resmca$var$v.contrib) = dimnames(eta2)
    
  resmca$oldcall <- resmca$call
  
  resmca$call = list()
  resmca$call$X <- resmca$mycall$X
  if(is.character(resmca$mycall$excl)) {
    excl <- which(getindexcat(resmca$mycall$X) %in% resmca$mycall$excl)
  } else {
    excl <- resmca$mycall$excl
    }
  resmca$call$excl <- excl
  resmca$call$excl.char <- getindexcat(resmca$mycall$X)[excl] 
  resmca$call$row.w <- resmca$mycall$row.w
  resmca$call$ncp <- resmca$oldcall$ncp
  
  resmca$ind <- resmca$row.sup
  colnames(resmca$ind$coord) <- gsub("Dim ","dim.", colnames(resmca$ind$coord))
  colnames(resmca$ind$cos2) <- gsub("Dim ","dim.", colnames(resmca$ind$cos2))
  
  oldeig <- resmca$eig
  resmca$eig <- list()
  resmca$eig$eigen <- oldeig[,1]
  resmca$eig$rate <- oldeig[,2]
  resmca$eig$cum.rate <- oldeig[,3]
  resmca$eig$mrate <- oldeig[,2]
  resmca$eig$cum.mrate <- oldeig[,3]
  
  attr(resmca,'class')[1] <- "bcMCA"
  attr(resmca,'class')[2] <- "CA"

  return(resmca)
}
