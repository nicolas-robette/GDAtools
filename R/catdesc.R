#' @importFrom moreparty BivariateAssoc

catdesc <- function(y,x,min.phi=NULL) {
  xcat <- x[,sapply(x,is.factor)]
  xcat <- as.data.frame(xcat)
  xcat.dic <- dichotom(xcat)
  xcon <- x[,sapply(x,is.numeric) | sapply(x,is.integer)]
  xcon <- as.data.frame(xcon)
  resbycat <- list()
  for(i in 1:nlevels(y)) {
    pct.ycat.in.xcat <- numeric()
    pct.xcat.in.ycat <- numeric()
    pct.xcat.global <- numeric()
    phi <- numeric()
    for(j in 1:ncol(xcat.dic)) {
      tab22 <- table(y==levels(y)[i], xcat.dic[,j])
      khi2 <- chisq.test(tab22, simulate.p.value=T, B=2)$statistic
      signe <- sign(tab22[2,2]/rowSums(tab22)[2]-tab22[1,2]/rowSums(tab22)[1])
      phi[j] <- round(signe*sqrt(khi2/sum(tab22)),3)
      pct.ycat.in.xcat[j] <- prop.table(tab22,2)[2,2]
      pct.xcat.in.ycat[j] <- prop.table(tab22,1)[2,2]
      pct.xcat.global[j] <- colSums(prop.table(tab22))[2]
    }
    categories <- data.frame(categories=names(xcat.dic),pct.ycat.in.xcat,pct.xcat.in.ycat,pct.xcat.global,phi)
    categories <- categories[order(-categories$phi),]
    if(!is.null(min.phi)) categories <- categories[abs(categories$phi)>=min.phi,]
    
    corr.coef <- numeric()
    median.x.in.ycat <- numeric()
    median.x.global <- numeric()
    sd.x.in.ycat <- numeric()
    sd.x.global <- numeric()
    for(j in 1:ncol(xcon)) {
      corr.coef[j] <- cor.test(as.numeric(y==levels(y)[i]), xcon[,j], method='pearson')$estimate
      median.x.in.ycat[j] <- median(xcon[y==levels(y)[i],j],na.rm=TRUE)
      median.x.global[j] <- median(xcon[,j],na.rm=TRUE)
      sd.x.in.ycat[j] <- sd(xcon[y==levels(y)[i],j],na.rm=TRUE)
      sd.x.global[j] <- sd(xcon[,j],na.rm=TRUE)
    }
    continuous.var <- data.frame(variables=names(xcon),median.x.in.ycat,median.x.global,sd.x.in.ycat,sd.x.global,corr.coef)
    continuous.var <- continuous.var[order(-continuous.var$corr.coef),]
    
    resbycat[[i]] <- list(categories=categories,continuous.var=continuous.var)
  }
  names(resbycat) <- levels(y)
  res <- list(variables=moreparty::BivariateAssoc(y,x,xx=FALSE)$YX, bylevel=resbycat)
  return(res)
}