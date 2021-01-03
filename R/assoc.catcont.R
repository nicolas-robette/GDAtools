assoc.catcont <- function(x,y,nperm=1000,distrib="asympt") {
  eta.squared <- summary.lm(aov(y~x))$r.squared
  
  if(!is.null(nperm)) {
    h0distrib <- numeric()
    for(i in 1:nperm) h0distrib[i] <- summary.lm(aov(sample(y)~x))$r.squared
    if(distrib=='approx') {
      permutation.pvalue <- sum(eta.squared<=h0distrib)/nperm
    } else {
      fit <- MASS::fitdistr(h0distrib,"normal")$estimate
      permutation.pvalue <- 1-stats::pnorm(eta.squared,fit[1],fit[2])   
    }
  }
  if(is.null(nperm)) permutation.pvalue <- NULL
  
  cor.coeff <- numeric(length=nlevels(x))
  for(i in 1:nlevels(x)) cor.coeff[i] <- cor.test(as.numeric(x==levels(x)[i]), y, method='pearson')$estimate
  names(cor.coeff) <- levels(x)
  cor.coeff <- round(cor.coeff,3)
  return(list('eta.squared'=eta.squared, 'permutation.pvalue'=permutation.pvalue, 'cor.coeff'=cor.coeff))
}
