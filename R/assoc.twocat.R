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

  phi <- phi.table(x,y)
    
  t <- table(x,y)
  cramer.v <- suppressWarnings(sqrt(chisq.test(t)$statistic / (length(Y)*(min(nrow(t),ncol(t))-1))))
  names(cramer.v) <- NULL
  
  return(list('freq'=freq, 'prop'=prop, 'rprop'=rprop, 'cprop'=cprop, 'cramer.v'=cramer.v, 'phi'=phi))
}

