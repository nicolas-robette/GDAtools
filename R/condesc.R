#' @importFrom moreparty BivariateAssoc

condesc <- function(y,x,min.cor=NULL){
  # xcat <- x[,sapply(x,is.factor)]
  # xcat <- as.data.frame(xcat)
  icat <- which(sapply(x,is.factor))
  xcat <- as.data.frame(x[,icat])
  names(xcat) <- names(x)[icat]
  xcat.dic <- dichotom(xcat)
  corr.coef <- numeric()
  median.y.in.xcat <- numeric()
  median.y.global <- numeric()
  sd.y.in.xcat <- numeric()
  sd.y.global <- numeric()
  for(i in 1:ncol(xcat.dic)) {
    corr.coef[i] <- cor.test(y, xcat.dic[,i], method='pearson')$estimate
    median.y.in.xcat[i] <- median(y[xcat.dic[,i]==1],na.rm=TRUE)
    median.y.global[i] <- median(y,na.rm=TRUE)
    sd.y.in.xcat[i] <- sd(y[xcat.dic[,i]==1],na.rm=TRUE)
    sd.y.global[i] <- sd(y,na.rm=TRUE)
  }
  categories <- data.frame(categories=names(xcat.dic),median.y.in.xcat,median.y.global,sd.y.in.xcat,sd.y.global,corr.coef)
  categories <- categories[order(-categories$corr.coef),]
  categories$corr.coef <- round(categories$corr.coef,3)
  if(!is.null(min.cor)) categories <- categories[abs(categories$corr.coef)>=min.cor,]
  res <- list(variables=moreparty::BivariateAssoc(y,x,xx=FALSE)$YX, categories=categories)
  return(res)
}