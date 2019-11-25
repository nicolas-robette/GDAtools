assoc.catcat <- function(x,y) {
  x <- factor(x)
  y <- factor(y)  # to drop empty levels
  phi <- matrix(NA, nrow=nlevels(x), ncol=nlevels(y))
  for(i in 1:nlevels(x)) {
    for(j in 1:nlevels(y)) {
      tab22 <- table(x==levels(x)[i], y==levels(y)[j])
      khi2 <- chisq.test(tab22, simulate.p.value=T, B=2)$statistic
      signe <- sign(tab22[2,2]/rowSums(tab22)[2]-tab22[1,2]/rowSums(tab22)[1])
      phi[i,j] <- round(signe*sqrt(khi2/sum(tab22)),3)
    }
  }
  rownames(phi) <- levels(x)
  colnames(phi) <- levels(y)
  tab <- table(x,y)
  khi2 <- chisq.test(tab, simulate.p.value=T, B=2)$statistic
  v2.cramer <- khi2 / (sum(tab)*min(nlevels(x)-1,nlevels(y)-1))
  names(v2.cramer) <- NULL
  return(list('v2.cramer'=v2.cramer, 'phi'=phi))
}