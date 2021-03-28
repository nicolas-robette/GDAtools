darma <- function(y,x,weights=rep(1,length(y)),target=1,twocont="kendall",nperm=1000,distrib="asympt",dec.a=3,dec.p=3) {
  
  weighted.quantile <- function(x, w, probs = .5, method = "raw") {
    if(method=="raw") {
      w <- w[order(x)]
      x <- x[order(x)]
      Fx = cumsum(w)/sum(w)
      rang <- max(which(Fx<probs))
      res <- x[rang] + (0.5 - Fx[rang])/(Fx[rang+1] - Fx[rang]) * (x[rang+1] - x[rang])
    }
    if(method=="density") {
      res <- with(density(x, weights = w/sum(w), n = 4096), 
                  x[which.max(cumsum(y*(x[2L] - x[1L])) >= probs)])
    }
    return(res)
  }
  
  ldf <- list()
  for(i in 1:ncol(x)) {
    if(is.factor(y) & is.factor(x[,i])) {
      biv <- GDAtools::assoc.twocat(y,x[,i],weights=weights,nperm=nperm,distrib=distrib)
      pct <- biv$cprop[target,1:nlevels(x[,i])]
      assoc <- biv$phi[target,]
      pval <- biv$phi.perm.pval[target,]
      var <- c(names(x)[i], rep("",nlevels(x[,i])-1))
      mod <- names(assoc)
      ldf[[i]] <- data.frame(variable=var,category=mod,percent=pct,association=assoc,perm.pvalue=pval, stringsAsFactors = FALSE)
    }
    if(is.factor(y) & is.numeric(x[,i])) {
      biv <- GDAtools::assoc.catcont(y,x[,i],weights=weights,nperm=nperm,distrib=distrib)
      ldf[[i]] <- data.frame(variable=names(x)[i],category="",percent=NA,
                             association=biv$cor.coef[target],perm.pvalue=biv$permutation.pvalue,
                             stringsAsFactors = FALSE)
    }
    if(is.numeric(y) & is.factor(x[,i])) {
      biv <- GDAtools::assoc.catcont(x[,i],y,weights=weights,nperm=nperm,distrib=distrib)
      # med <- sapply(split(data.frame(y,weights),x[,i]), function(X) weighted.mean(X[,1],X[,2]))
      med <- round(sapply(split(data.frame(y,weights),x[,i]), function(X) weighted.quantile(X[,1],X[,2],probs=.5,method="density")),2)
      assoc <- biv$cor.coeff
      pval <- biv$cor.perm.pval
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
  res$association <- round(res$association, dec.a)
  res$perm.pvalue <- round(res$perm.pvalue, dec.p)
  return(res)
}
