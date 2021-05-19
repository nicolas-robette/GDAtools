catdesc <- function(y,x,weights=rep(1,length(y)),min.phi=NULL,robust=TRUE,nperm=NULL,distrib="asympt",dec=c(3,3,3,3,1,3)) {
  
  icat <- which(sapply(x,is.factor))
  xcat <- as.data.frame(x[,icat])
  names(xcat) <- names(x)[icat]
  icon <- which(sapply(x, function(x) is.numeric(x) | is.integer(x)))
  xcon <- as.data.frame(x[,icon])
  names(xcon) <- names(x)[icon]
  
  if(ncol(xcat)==0) {
    lcat <- lapply(levels(y), function(x) return(NULL))
    names(lcat) <- levels(y)
  }
  
  if(ncol(xcat)>0) {
    lcat <- list()
    for(i in 1:ncol(xcat)) {
      temp <- assoc.twocat(y, xcat[,i], weights=weights, nperm=NULL)$gather
      temp$categories <- paste(names(xcat)[i],temp$Var2,sep='.')
      lcat[[i]] <- merge(temp, aggregate(prop~Var2, data=temp, sum), by="Var2")
    }
    lcat <- do.call("rbind.data.frame",lcat)
    lcat <- lcat[order(-lcat$phi),]
    if(!is.null(min.phi)) lcat <- lcat[abs(lcat$phi)>=min.phi,]
    lcat$cprop <- round(lcat$cprop,dec[3])
    lcat$rprop <- round(lcat$rprop,dec[3])
    lcat$prop.y <- round(lcat$prop.y,dec[3])
    lcat$phi <- round(lcat$phi,dec[4])
    splitvar <- lcat$Var1
    lcat <- lcat[,c("categories","cprop","rprop","prop.y","phi")]
    names(lcat) <- c("categories","pct.ycat.in.xcat","pct.xcat.in.ycat","pct.xcat.global","phi")
    rownames(lcat) <- NULL
    lcat <- split(lcat, splitvar)
  }

  if(ncol(xcon)==0) {
    lcon <- lapply(levels(y), function(x) return(NULL))
    names(lcon) <- levels(y)
  }
    
  if(ncol(xcon)>0) {
    lcon <- list()
    for(i in 1:ncol(xcon)) {
      temp <- data.frame(cor = assoc.catcont(y, xcon[,i], weights=weights, nperm=NULL, digits=9)$cor)
      temp$variables <- rep(names(xcon)[i],nrow(temp))
      temp$categories <- rownames(temp)
      if(robust==TRUE) {
        temp$median.x.in.ycat <- sapply(levels(y), function(x) weighted.quantile(xcon[y==x,i], weights[y==x], method="density"))
        temp$median.x.global <- rep(weighted.quantile(xcon[,i], weights, method="density"),nrow(temp))
        temp$mad.x.in.ycat <- sapply(levels(y), function(x) weighted.mad(xcon[y==x,i], weights[y==x], method="density"))
        temp$mad.x.global <- rep(weighted.mad(xcon[,i], weights, method="density"),nrow(temp))
      }
      if(robust==FALSE) {
        temp$median.x.in.ycat <- sapply(levels(y), function(x) weighted.mean(xcon[y==x,i], weights[y==x]))
        temp$median.x.global <- rep(weighted.mean(xcon[,i], weights),nrow(temp))
        temp$mad.x.in.ycat <- sapply(levels(y), function(x) weighted.sd(xcon[y==x,i], weights[y==x]))
        temp$mad.x.global <- rep(weighted.sd(xcon[,i], weights),nrow(temp))       
      }
      lcon[[i]] <- temp
    }
    lcon <- do.call("rbind.data.frame",lcon)
    lcon <- lcon[order(-lcon$cor),]
    lcon$median.x.in.ycat <- round(lcon$median.x.in.ycat, dec[5])
    lcon$median.x.global <- round(lcon$median.x.global, dec[5])
    lcon$mad.x.in.ycat <- round(lcon$mad.x.in.ycat, dec[5])
    lcon$mad.x.global <- round(lcon$mad.x.global, dec[5])
    lcon$cor <- round(lcon$cor,dec[6])
    splitvar <- lcon$categories
    lcon <- lcon[,c(2,4:7,1)]
    if(robust==FALSE) names(lcon) <- c("variables","mean.x.in.ycat","mean.x.global",
                                       "sd.x.in.ycat","sd.x.global","cor")
    rownames(lcon) <- NULL
    lcon <- split(lcon,splitvar)
  }
  
  bylevel <- list()
  for(i in levels(y)) {
    bylevel[[i]]$categories <- lcat[[i]]
    bylevel[[i]]$continuous.var <- lcon[[i]]
  }
  
  res <- list(variables=assoc.yx(y,x,weights=weights,xx=FALSE,nperm=nperm,distrib=distrib,dec=dec[1:2])$YX, bylevel=bylevel)
  return(res)
}
