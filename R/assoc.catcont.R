assoc.catcont <- function(x,y,weights=rep(1,length(x)),nperm=1000,distrib="asympt",digits=3) {

  eta.squared <- summary.lm(aov(y~x,weights=weights))$r.squared
  
  if(!is.null(nperm)) {
    h0distrib <- numeric()
    for(i in 1:nperm) h0distrib[i] <- summary.lm(aov(sample(y)~x,weights=weights))$r.squared
    if(distrib=='approx') {
      permutation.pvalue <- sum(eta.squared<=h0distrib)/nperm
    } else {
      fit <- MASS::fitdistr(h0distrib,"normal")$estimate
      permutation.pvalue <- 1-stats::pnorm(eta.squared,fit[1],fit[2])   
    }
  }
  if(is.null(nperm)) permutation.pvalue <- NULL
  
  cor.coeff <- numeric(length=nlevels(x))
  # for(i in 1:nlevels(x)) cor.coeff[i] <- cor.test(as.numeric(x==levels(x)[i]), y, method='pearson')$estimate
  for(i in 1:nlevels(x)) cor.coeff[i] <- wdm::wdm(as.numeric(x==levels(x)[i]), y, 
                                                  method="pearson", weights=weights, remove_missing=TRUE)
  names(cor.coeff) <- levels(x)
  
  if(!is.null(nperm)) {
    ppval <- numeric(length=nlevels(x))
    for(i in 1:nlevels(x)) {
      obs <- cor.coeff[i]
      h0d <- numeric()
      for(j in 1:nperm) h0d[j] <- wdm::wdm(as.numeric(x==levels(x)[i]), sample(y), 
                                           method="pearson", weights=weights, remove_missing=TRUE)
      if(distrib=='approx') {
        if(obs>=0) ppval[i] <- sum(obs<=h0d)/nperm
        if(obs<0) ppval[i] <- sum(obs>h0d)/nperm
      } else {
        fit <- MASS::fitdistr(h0d,"normal")$estimate
        if(obs>=0) ppval[i] <- 1-stats::pnorm(obs,fit[1],fit[2])
        if(obs<0) ppval[i] <- stats::pnorm(obs,fit[1],fit[2])
      }
    }
    names(ppval) <- levels(x)
  }
  if(is.null(nperm)) ppval <- NULL

  cor.coeff <- round(cor.coeff,digits)
  return(list('eta.squared'=eta.squared, 'permutation.pvalue'=permutation.pvalue, 'cor.coeff'=cor.coeff, 'cor.perm.pval'=ppval))
}
