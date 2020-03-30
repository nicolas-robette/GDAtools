
assoc.twocat <- function(x, y, w=rep.int(1,length(x)), na=TRUE) {

  x <- factor(x)
  y <- factor(y)  # to drop empty levels
  if(na) x <- addNA(x, TRUE)
  if(na) y <- addNA(y, TRUE)
  
  xdic <- as.matrix(dichotom(x, out='numeric'))
  ydic <- as.matrix(dichotom(y, out='numeric'))
  tab <- t(xdic)%*%diag(w)%*%ydic
  tab <- as.table(tab)
  rownames(tab) <- gsub('data.','',rownames(tab))
  colnames(tab) <- gsub('data.','',colnames(tab))
  
  freq <- addmargins(tab)
  prop <- round(400*prop.table(freq),1)
  rprop <- round(100*apply(freq, 2, function(x) 2*x/rowSums(freq)),1)
  cprop <- t(round(100*apply(freq, 1, function(x) 2*x/colSums(freq)),1))

  phi <- matrix(NA, nrow=nlevels(x), ncol=nlevels(y))
  for(i in 1:nlevels(x)) {
    for(j in 1:nlevels(y)) {
      # tab22 <- table(x==levels(x)[i], y==levels(y)[j])
      x2dic <- as.matrix(dichotom(x==levels(x)[i], out='numeric'))
      y2dic <- as.matrix(dichotom(y==levels(y)[j], out='numeric'))
      tab22 <- t(x2dic)%*%diag(w)%*%y2dic
      tab22 <- as.table(tab22)  
      khi2 <- chisq.test(tab22, simulate.p.value=T, B=2)$statistic
      signe <- sign(tab22[2,2]/rowSums(tab22)[2]-tab22[1,2]/rowSums(tab22)[1])
      phi[i,j] <- round(signe*sqrt(khi2/sum(tab22)),3)
    }
  }
  rownames(phi) <- levels(x)
  colnames(phi) <- levels(y)
  #tab <- table(x,y)
  
  khi2 <- chisq.test(tab, simulate.p.value=T, B=2)$statistic
  v2.cramer <- khi2 / (sum(tab)*min(nlevels(x)-1,nlevels(y)-1))
  names(v2.cramer) <- NULL
  
  return(list('freq'=freq, 'prop'=prop, 'rprop'=rprop, 'cprop'=cprop, 'v2.cramer'=v2.cramer, 'phi'=phi))
}