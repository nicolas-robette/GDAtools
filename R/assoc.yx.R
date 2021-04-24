assoc.yx <- function(y,x,weights=rep(1,length(y)),xx=TRUE,twocont="kendall",nperm=NULL,distrib="asympt",dec=c(3,3)) {
  
  x <- as.data.frame(x)
  xnames <- names(x)
  xformats <- sapply(x,class)
  yformat <- class(y)
  
  res <- list()
  for(i in 1:ncol(x)) {
    # print(i)
    if(yformat=='numeric' & xformats[i] %in% c('numeric','integer')) {
      z <- GDAtools::assoc.twocont(y, x[,i], weights=weights, nperm=nperm, distrib=distrib)
      measure = twocont
      association = z[,twocont][1]
      permutation.pvalue = z[,twocont][2]
    }
    if(yformat=='numeric' & xformats[i]=='factor') {
      z <- GDAtools::assoc.catcont(x[,i], y, weights=weights, nperm=nperm, distrib=distrib)
      measure='Eta2'
      association = z$eta.squared
      permutation.pvalue = z$permutation.pvalue
    }
    if(yformat=='factor' & xformats[i]%in% c('numeric','integer')) {
      z <- GDAtools::assoc.catcont(y, x[,i], weights=weights, nperm=nperm, distrib=distrib)
      measure='Eta2'
      association = z$eta.squared
      permutation.pvalue = z$permutation.pvalue
    }
    if(yformat=='factor' & xformats[i]=='factor') {
      z <- GDAtools::assoc.twocat(x[,i], y, weights=weights, nperm=nperm, distrib=distrib)
      measure="Cramer's V"
      association = z$cramer.v
      permutation.pvalue = z$permutation.pvalue
    }
    if(is.null(nperm)) permutation.pvalue <- NA
    res[[i]] <- data.frame(measure,association,permutation.pvalue, stringsAsFactors = F)
  }
  res <- do.call('rbind.data.frame',res)
  restot <- data.frame(variable=xnames,measure=res$measure,association=res$association,permutation.pvalue=res$permutation.pvalue)
  restot <- restot[order(restot$permutation.pvalue,restot$measure,-restot$association),]
  restot$measure <- gsub("kendall","Kendall's tau",restot$measure)
  restot$measure <- gsub("spearman","Spearman's rho",restot$measure)
  restot$measure <- gsub("pearson","Pearson's r",restot$measure)
  rownames(restot) <- NULL
  restot$association <- round(restot$association,dec[1])
  restot$permutation.pvalue <- round(restot$permutation.pvalue,dec[2])
  
  if(xx==TRUE) {
    combi <- utils::combn(xnames,2,simplify=F)
    res <- list()
    for(i in 1:length(combi)) {
      x1 <- x[,combi[[i]][1]]
      x2 <- x[,combi[[i]][2]]
      
      if(class(x1) %in% c('numeric','integer') & class(x2) %in% c('numeric','integer')) {
        z <- GDAtools::assoc.twocont(x1, x2, weights=weights, nperm=nperm, distrib=distrib)
        measure = twocont
        association = z[,twocont][1]
        permutation.pvalue = z[,twocont][2]
      }
      if(class(x1) %in% c('numeric','integer') & class(x2)=='factor') {
        z <- GDAtools::assoc.catcont(x2, x1, weights=weights, nperm=nperm, distrib=distrib)
        measure='Eta2'
        association = z$eta.squared
        permutation.pvalue = z$permutation.pvalue
      }
      if(class(x1)=='factor' & class(x2) %in% c('numeric','integer')) {
        z <- GDAtools::assoc.catcont(x1, x2, weights=weights, nperm=nperm, distrib=distrib)
        measure='Eta2'
        association = z$eta.squared
        permutation.pvalue = z$permutation.pvalue
      }
      if(class(x1)=='factor' & class(x2)=='factor') {
        z <- GDAtools::assoc.twocat(x1, x2, weights=weights, nperm=nperm, distrib=distrib)
        measure="Cramer's V"
        association = z$cramer.v
        permutation.pvalue = z$permutation.pvalue
      }
      if(is.null(nperm)) permutation.pvalue <- NA
      res[[i]] <- data.frame(measure,association,permutation.pvalue, stringsAsFactors = F)
    }
    res <- do.call('rbind.data.frame',res)
    noms <- do.call('rbind.data.frame',combi)
    restot2 <- data.frame(variable1=noms[,1],variable2=noms[,2],measure=res$measure,association=res$association,permutation.pvalue=res$permutation.pvalue,row.names=NULL)
    restot2 <- restot2[order(restot2$permutation.pvalue,restot2$measure,-restot2$association),]
    restot2$measure <- gsub("kendall","Kendall's tau",restot2$measure)
    restot2$measure <- gsub("spearman","Spearman's rho",restot2$measure)
    restot2$measure <- gsub("pearson","Pearson's r",restot2$measure)
    rownames(restot2) <- NULL
    restot2$association <- round(restot2$association,dec[1])
    restot2$permutation.pvalue <- round(restot2$permutation.pvalue,dec[2])
  } else {
    restot2 <- NULL
  }

  return(list(YX=restot, XX=restot2))
}
