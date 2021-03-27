wtable <- function(x, y=NULL, weights=rep.int(1,length(x)), stat="freq", digits=1, mar=TRUE, na_value=NULL) {
  
  # add na level
  if(!is.null(na_value)) {
    x <- factor(x, levels=c(levels(x), na_value))
    x[is.na(x)] <- na_value
    x <- factor(x)
    if(!is.null(y)) {
      y <- factor(y, levels=c(levels(y), na_value))
      y[is.na(y)] <- na_value
      y <- factor(y)
    }
  }
  
  # remove obs with na
  if(!is.null(y)) {
    idnona <- !is.na(x) & !is.na(y)
    X <- x[idnona]
    Y <- y[idnona]
    W <- weights[idnona]    
  } else {
    idnona <- !is.na(x)
    X <- x[idnona]
    W <- weights[idnona]    
  }

  if(!is.null(y)) {
    t <- tapply(W, list(X,Y), sum)
    tab <- as.table(t)
    if(mar) tab <- addmargins(tab)
    if(stat=="prop") {
      tab <- 100*prop.table(tab)
      if(mar) tab <- 4*tab
    }
    if(stat=="rprop") {
      tab <- 100*apply(tab, 2, function(x) x/rowSums(tab))
      if(mar) tab <- 2*tab
    }
    if(stat=="cprop") {
      tab <- t(100*apply(tab, 1, function(x) x/colSums(tab)))
      if(mar) tab <- 2*tab}
  } else {
    t <- tapply(W, list(X), sum)
    tab <- as.table(t)
    if(mar) tab <- addmargins(tab)
    if(stat=="prop") {
      tab <- 100*prop.table(tab)
      if(mar) tab <- 2*tab
    }
  }

  if(!is.null(digits)) tab <- round(tab, digits)
  
  return(tab)
  }
