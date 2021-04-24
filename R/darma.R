darma <- function(y,x,weights=rep(1,length(y)),target=1,twocont="kendall",nperm=NULL,distrib="asympt",dec=c(1,3,3)) {

  x <- as.data.frame(x)
    
  ldf <- list()
  for(i in 1:ncol(x)) {
    if(is.factor(y) & is.factor(x[,i])) {
      biv <- GDAtools::assoc.twocat(y,x[,i],weights=weights,nperm=nperm,distrib=distrib)
      pct <- biv$cprop[target,1:nlevels(x[,i])]
      assoc <- biv$phi[target,]
      pval <- biv$phi.perm.pval[target,]
      if(is.null(nperm)) pval <- rep(NA,length(assoc))
      var <- c(names(x)[i], rep("",nlevels(x[,i])-1))
      mod <- names(assoc)
      ldf[[i]] <- data.frame(variable=var,category=mod,percent=pct,association=assoc,perm.pvalue=pval, stringsAsFactors = FALSE)
    }
    if(is.factor(y) & is.numeric(x[,i])) {
      biv <- GDAtools::assoc.catcont(y,x[,i],weights=weights,nperm=nperm,distrib=distrib)
      pval <- biv$permutation.pvalue
      if(is.null(nperm)) pval <- NA
      ldf[[i]] <- data.frame(variable=names(x)[i],category="",percent=NA,
                             association=biv$cor[target],perm.pvalue=pval,
                             stringsAsFactors = FALSE)
    }
    if(is.numeric(y) & is.factor(x[,i])) {
      biv <- GDAtools::assoc.catcont(x[,i],y,weights=weights,nperm=nperm,distrib=distrib)
      # med <- sapply(split(data.frame(y,weights),x[,i]), function(X) weighted.mean(X[,1],X[,2]))
      med <- round(sapply(split(data.frame(y,weights),x[,i]), function(X) weighted.quantile(X[,1],X[,2],probs=.5,method="density")),2)
      assoc <- biv$cor
      pval <- biv$cor.perm.pval
      if(is.null(nperm)) pval <- rep(NA,length(assoc))
      var <- c(names(x)[i], rep("",nlevels(x[,i])-1))
      mod <- names(assoc)
      ldf[[i]] <- data.frame(variable=var,category=mod,median=med,
                             association=assoc,perm.pvalue=pval,
                             stringsAsFactors = FALSE)
    }
    if(is.numeric(y) & is.numeric(x[,i])) {
      biv <- GDAtools::assoc.twocont(y,x[,i],weights=weights,nperm=nperm,distrib=distrib)
      ldf[[i]] <- data.frame(variable=names(x)[i],category="",median=NA,
                             association=biv[1,twocont],perm.pvalue=biv[2,twocont],
                             stringsAsFactors = FALSE)
    }
  }
  res <- do.call('rbind.data.frame', ldf)
  rownames(res) <- NULL
  res[,3] <- round(res[,3], dec[1])
  res$association <- round(res$association, dec[2])
  res$perm.pvalue <- round(res$perm.pvalue, dec[3])
  return(res)
}
