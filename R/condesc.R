condesc <- function(y,x,weights=rep(1,length(y)),min.cor=NULL,robust=TRUE,nperm=NULL,distrib="asympt",dec=c(3,3,0,3)){

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
    if(robust==TRUE) {
      median.y.in.xcat[i] <- weighted.quantile(rep(y[xcat.dic[,i]==1],2), w=rep(weights[xcat.dic[,i]==1],2), method="density")
      median.y.global[i] <- weighted.quantile(y, w=weights, method="density")
      mad.y.in.xcat[i] <- weighted.mad(rep(y[xcat.dic[,i]==1],2), w=rep(weights[xcat.dic[,i]==1],2), method="density")
      mad.y.global[i] <- weighted.mad(y, w=weights, method="density")
    }
    if(robust==FALSE) {
      median.y.in.xcat[i] <- weighted.mean(rep(y[xcat.dic[,i]==1],2), w=rep(weights[xcat.dic[,i]==1],2))
      median.y.global[i] <- weighted.mean(y, w=weights)
      mad.y.in.xcat[i] <- weighted.sd(rep(y[xcat.dic[,i]==1],2), w=rep(weights[xcat.dic[,i]==1],2))
      mad.y.global[i] <- weighted.sd(y, w=weights)      
    }
  }
  
  categories <- data.frame(categories=names(xcat.dic),
                           median.y.in.xcat=round(median.y.in.xcat,dec[3]),
                           median.y.global=round(median.y.global,dec[3]),
                           mad.y.in.xcat=round(mad.y.in.xcat,dec[3]),
                           mad.y.global=round(mad.y.global,dec[3]),
                           cor=round(cor,dec[4]))
  if(robust==FALSE) names(categories) <- c("categories","mean.y.in.xcat","mean.y.global",
                                           "sd.y.in.xcat","sd.y.global","cor")
  categories <- categories[order(-categories$cor),]
  if(!is.null(min.cor)) categories <- categories[abs(categories$cor)>=min.cor,]
  
  res <- list(variables=GDAtools::assoc.yx(y,x,xx=FALSE,weights=weights,nperm=nperm,distrib=distrib,dec=dec[1:2])$YX, categories=categories)
  return(res)
}
