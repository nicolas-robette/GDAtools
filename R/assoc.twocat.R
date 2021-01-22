assoc.twocat <- function(x, y, weights=rep.int(1,length(x)), na=TRUE, nperm=1000, distrib="asympt") {

  x <- factor(x)
  y <- factor(y)  # to drop empty levels
  if(na) x <- addNA(x, TRUE)
  if(na) y <- addNA(y, TRUE)
  
  xdic <- as.matrix(GDAtools::dichotom(x, out='numeric'))
  ydic <- as.matrix(GDAtools::dichotom(y, out='numeric'))
  tab <- t(xdic)%*%diag(weights)%*%ydic
  tab <- as.table(tab)
  rownames(tab) <- gsub('data.','',rownames(tab))
  colnames(tab) <- gsub('data.','',colnames(tab))
  
  freq <- addmargins(tab)
  prop <- round(400*prop.table(freq),1)
  rprop <- round(100*apply(freq, 2, function(x) 2*x/rowSums(freq)),1)
  cprop <- t(round(100*apply(freq, 1, function(x) 2*x/colSums(freq)),1))

  phi <- GDAtools::phi.table(x,y,weights=weights,digits=NULL)
  
  # pem <- GDAtools::pem(x,y,weights=weights)

  t <- t(xdic)%*%diag(weights)%*%ydic
  expected <- rowSums(t) %*% t(colSums(t)) / sum(t)
  chi.squared <- sum((t-expected)*(t-expected)/expected)
  cramer.v <- sqrt(chi.squared / (length(x)*(min(nrow(t),ncol(t))-1)))
  expected <- as.table(expected)
  dimnames(expected) <- dimnames(t)
  
  stdres <- (t-expected)/sqrt(expected)
  stdres <- as.table(stdres)
  
  if(!is.null(nperm)) {
    h0distrib <- numeric()
    for(i in 1:nperm) {
      permt <- table(x,sample(y))
      permexp <- rowSums(permt) %*% t(colSums(permt)) / sum(permt)
      h0distrib[i] <- sum((permt-permexp)*(permt-permexp)/permexp)
    }
    if(distrib=='approx') {
      permutation.pvalue <- sum(chi.squared<=h0distrib)/nperm
    } else {
      fit <- MASS::fitdistr(h0distrib,"normal")$estimate
      permutation.pvalue <- 1-stats::pnorm(chi.squared,fit[1],fit[2])   
    }
  }
  if(is.null(nperm)) permutation.pvalue <- NULL

  if(!is.null(nperm)) {  
    ar <- array(NA, dim=c(nperm,nrow(phi),ncol(phi)))
    for(i in 1:nperm) ar[i,,] <- GDAtools::phi.table(x,sample(y),weights=weights,digits=NULL)
    ppval <- matrix(nrow=nrow(phi), ncol=ncol(phi))
    for(i in 1:nrow(ppval)) { 
      for(j in 1:ncol(ppval)) {
        h0 <- ar[,i,j]
        obs <- phi[i,j]
        if(distrib=='approx') {
          if(obs>=0) ppval[i,j] <- sum(obs<=h0)/nperm
          if(obs<0) ppval[i,j] <- sum(obs>h0)/nperm
        } else {
          fit <- MASS::fitdistr(h0,"normal")$estimate
          if(obs>=0) ppval[i,j] <- 1-stats::pnorm(obs,fit[1],fit[2])
          if(obs<0) ppval[i,j] <- stats::pnorm(obs,fit[1],fit[2])
        }
    }}
    ppval <- as.table(ppval)
    dimnames(ppval) <- dimnames(phi)
  }
  if(is.null(nperm)) ppval <- NULL
  
  return(list('freq'=freq, 'prop'=prop, 'rprop'=rprop, 'cprop'=cprop, 'expected'=expected, 'chi.squared'=chi.squared, 'cramer.v'=cramer.v, 'permutation.pvalue'=permutation.pvalue, 'pearson.residuals'=stdres, 'phi'=phi, 'phi.perm.pval'=ppval)) #, 'local.pem'=pem$peml, 'global.pem'=pem$pemg))
}
