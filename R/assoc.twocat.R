assoc.twocat <- function(x, y, w=rep.int(1,length(x)), na=TRUE, nperm=1000) {

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

  phi <- phi.table(x,y)
    
  t <- table(x,y)
  expected <- rowSums(t) %*% t(colSums(t)) / sum(t)
  chi.squared <- sum((t-expected)*(t-expected)/expected)
  cramer.v <- sqrt(chi.squared / (length(x)*(min(nrow(t),ncol(t))-1)))
  expected <- as.table(expected)
  dimnames(expected) <- dimnames(t)
  
  if(!is.null(nperm)) {
    h0distrib <- numeric()
    for(i in 1:nperm) {
      permt <- table(x,sample(y))
      permexp <- rowSums(permt) %*% t(colSums(permt)) / sum(permt)
      h0distrib[i] <- sum((permt-permexp)*(permt-permexp)/permexp)
    }
    permutation.pvalue <- 1-sum(chi.squared>h0distrib)/nperm
  }
  if(is.null(nperm)) permutation.pvalue <- NULL
  
  return(list('freq'=freq, 'prop'=prop, 'rprop'=rprop, 'cprop'=cprop, 'expected'=expected, 'chi.squared'=chi.squared,'cramer.v'=cramer.v, 'permutation.pvalue'=permutation.pvalue,'phi'=phi))
}
