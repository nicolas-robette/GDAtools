assoc.catcont <- function(x,y) {
  eta.squared <- round(summary.lm(aov(y~x))$r.squared,3)
  cor.coeff <- numeric(length=nlevels(x))
  for(i in 1:nlevels(x)) cor.coeff[i] <- cor.test(as.numeric(x==levels(x)[i]), y, method='pearson')$estimate
  names(cor.coeff) <- levels(x)
  cor.coeff <- round(cor.coeff,3)
  return(list('eta.squared'=eta.squared, 'cor.coeff'=cor.coeff))
}