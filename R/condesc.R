condesc <- function(y,x,weights=rep(1,length(y)),min.cor=NULL,nperm=10,distrib="asympt"){

  icat <- which(sapply(x,is.factor))
  xcat <- as.data.frame(x[,icat])
  names(xcat) <- names(x)[icat]
  xcat.dic <- dichotom(xcat)
  
  cor.coef <- numeric()
  median.y.in.xcat <- numeric()
  median.y.global <- numeric()
  mad.y.in.xcat <- numeric()
  mad.y.global <- numeric()
  for(i in 1:ncol(xcat.dic)) {
    cor.coef[i] <- wdm::wdm(y, xcat.dic[,i], weights=weights)
    median.y.in.xcat[i] <- weighted.quantile(rep(y[xcat.dic[,i]==1],2), w=rep(weights[xcat.dic[,i]==1],2), method="density")
    median.y.global[i] <- weighted.quantile(y, w=weights, method="density")
    mad.y.in.xcat[i] <- weighted.mad(rep(y[xcat.dic[,i]==1],2), w=rep(weights[xcat.dic[,i]==1],2), method="density")
    mad.y.global[i] <- weighted.mad(y, w=weights, method="density")
  }
  
  categories <- data.frame(categories=names(xcat.dic),median.y.in.xcat,median.y.global,mad.y.in.xcat,mad.y.global,cor.coef)
  categories <- categories[order(-categories$cor.coef),]
  #categories$cor.coef <- round(categories$cor.coef,3)
  if(!is.null(min.cor)) categories <- categories[abs(categories$cor.coef)>=min.cor,]
  
  res <- list(variables=GDAtools::assoc.yx(y,x,xx=FALSE,weights=weights,nperm=nperm,distrib=distrib)$YX, categories=categories)
  return(res)
}
