pem <- function(x,y,weights=rep(1,length(x)),digits=1,sort=TRUE) {

  idnona <- !is.na(x) & !is.na(y)
  X <- x[idnona]
  Y <- y[idnona]
  W <- weights[idnona]
  
  # cont <- t(as.matrix(GDAtools::dichotom(X,out='numeric')))%*%diag(W)%*%as.matrix(GDAtools::dichotom(Y,out='numeric'))
  cont <- stats::xtabs(data = data.frame(X, Y, W), W~X+Y)
  tota <- colSums(cont)
  totb <- rowSums(cont)
  total <- sum(cont)
  theo <- matrix(nrow=nrow(cont),ncol=ncol(cont))
  for(i in 1:nrow(cont)) { for(j in 1:ncol(cont)) theo[i,j] <- tota[j]*totb[i]/total }
  ecart <- cont-theo
  max <- matrix(nrow=nrow(cont),ncol=ncol(cont))
  emax <- matrix(nrow=nrow(cont),ncol=ncol(cont))
  pem <- matrix(nrow=nrow(cont),ncol=ncol(cont))
  for(i in 1:nrow(cont)) { for(j in 1:ncol(cont)) {
    if(ecart[i,j]>=0) max[i,j] <- min(tota[j],totb[i])
    if(ecart[i,j]<0 & tota[j]<=(total-totb[i])) max[i,j] <- 0
    if(ecart[i,j]<0 & tota[j]>(total-totb[i])) max[i,j] <- tota[j]+totb[i]-total
    emax[i,j] <- max[i,j] - theo[i,j]
    pem[i,j] <- ifelse(ecart[i,j]>=0,ecart[i,j]/emax[i,j]*100,0-ecart[i,j]/emax[i,j]*100)
    }}
  dimnames(pem) <- dimnames(cont)
  if(isFALSE(sort)) {
    z <- cont
  } else {
    old.warn <- options()$warn
    options(warn = -1)
    temp <- MASS::corresp(cont,nf=1)
    z <- cont[order(temp$rscore),order(temp$cscore)]
    options(warn = old.warn)
  }
  tota <- colSums(z)
  totb <- rowSums(z)
  maxc <- matrix(0,nrow=nrow(z),ncol=ncol(z))
  i <- 1; j <- 1
  repeat {
    m <- min(tota[j],totb[i])
    maxc[i,j] <- m
    tota[j] <- tota[j] - m
    totb[i] <- totb[i] - m
    if(sum(tota)+sum(totb)==0) break
    if(tota[j]==0) j <- j+1
    if(totb[i]==0) i <- i+1
  }
  
  if(isTRUE(sort)) {
    pemg <- (sum(ecart)+sum(abs(ecart)))/(sum(maxc-theo[order(temp$rscore),order(temp$cscore)])+sum(abs(maxc-theo[order(temp$rscore),order(temp$cscore)])))
  } else {
    pemg <- (sum(ecart)+sum(abs(ecart)))/(sum(maxc-theo)+sum(abs(maxc-theo)))
  }  
  pemg <- 100*pemg
  
  pem <- as.table(pem)
  # rownames(pem) <- gsub('data.','',rownames(pem))
  # colnames(pem) <- gsub('data.','',colnames(pem))
  
  if(!is.null(digits)) pem <- round(pem,digits)
  if(!is.null(digits)) pemg <- round(pemg,digits)
  
  PEM <- list(peml=pem, pemg=pemg)
  return(PEM)
}
