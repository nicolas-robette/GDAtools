assoc.twocont <- function(x,y,nperm=1000,distrib="asympt") {
  pearson <- cor(x,y,use="complete.obs",method="pearson")
  spearman <- cor(x,y,use="complete.obs",method="spearman")
  kendall <- cor(x,y,use="complete.obs",method="kendall")
  if(!is.null(nperm)) {
    h0P <- numeric()
    h0S <- numeric()
    h0K <- numeric()
    for(i in 1:nperm) {
      permy <- sample(y)
      h0P[i] <- cor(x,permy,use="complete.obs",method="pearson")
      h0S[i] <- cor(x,permy,use="complete.obs",method="spearman")
      h0K[i] <- cor(x,permy,use="complete.obs",method="kendall")
    }
    if(distrib=='approx') {
      pearson <- c(pearson, sum(pearson<=h0P)/nperm)
      spearman <- c(spearman, sum(spearman<=h0S)/nperm)
      kendall <- c(kendall, sum(kendall<=h0K)/nperm)
    } else {
      fit <- MASS::fitdistr(h0P,"normal")$estimate
      pearson <- c(pearson, 1-stats::pnorm(pearson,fit[1],fit[2]))
      fit <- MASS::fitdistr(h0S,"normal")$estimate
      spearman <- c(spearman, 1-stats::pnorm(spearman,fit[1],fit[2]))
      fit <- MASS::fitdistr(h0K,"normal")$estimate
      kendall <- c(kendall, 1-stats::pnorm(kendall,fit[1],fit[2]))
    }
  }
  res <- data.frame(pearson,spearman,kendall)
  rownames(res)[1] <- "value"
  if(!is.null(nperm)) rownames(res)[2] <- "permutation.pvalue"
  return(res)
}
