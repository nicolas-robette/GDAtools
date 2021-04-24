condesc <- function(y,x,weights=rep(1,length(y)),min.cor=NULL,nperm=NULL,distrib="asympt",dec=c(3,3,0,3)){

  icat <- which(sapply(x,is.factor))
  xcat <- as.data.frame(x[,icat])
  names(xcat) <- names(x)[icat]
  xcat.dic <- dichotom(xcat)
  
  cor <- numeric()
  median.y.in.xcat <- numeric()
  median.y.global <- numeric()
  mad.y.in.xcat <- numeric()
  mad.y.global <- numeric()
  for(i in 1:ncol(xcat.dic)) {
    cor[i] <- wdm::wdm(y, xcat.dic[,i], weights=weights)
    median.y.in.xcat[i] <- weighted.quantile(rep(y[xcat.dic[,i]==1],2), w=rep(weights[xcat.dic[,i]==1],2), method="density")
    median.y.global[i] <- weighted.quantile(y, w=weights, method="density")
    mad.y.in.xcat[i] <- weighted.mad(rep(y[xcat.dic[,i]==1],2), w=rep(weights[xcat.dic[,i]==1],2), method="density")
    mad.y.global[i] <- weighted.mad(y, w=weights, method="density")
  }
  
  categories <- data.frame(categories=names(xcat.dic),
                           median.y.in.xcat=round(median.y.in.xcat,dec[3]),
                           median.y.global=round(median.y.global,dec[3]),
                           mad.y.in.xcat=round(mad.y.in.xcat,dec[3]),
                           mad.y.global=round(mad.y.global,dec[3]),
                           cor=round(cor,dec[4]))
  categories <- categories[order(-categories$cor),]
  if(!is.null(min.cor)) categories <- categories[abs(categories$cor)>=min.cor,]
  
  res <- list(variables=GDAtools::assoc.yx(y,x,xx=FALSE,weights=weights,nperm=nperm,distrib=distrib,dec=dec[1:2])$YX, categories=categories)
  return(res)
}
