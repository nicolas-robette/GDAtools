dimtypicality <- function(resmca,vars,dim=c(1,2),max.pval=1) {
  n=names(vars)
  res1 <- list()
  res2 <- list()
  for(i in 1:length(vars)) {
    temp <- supvar(resmca,vars[[i]])
    res1[[i]] <- temp$typic
    rownames(res1[[i]]) <- paste(n[i],rownames(res1[[i]]),sep='.')
    res2[[i]] <- temp$weight
    }
  res1 <- do.call('rbind.data.frame',res1)
  res2 <- unlist(res2)
  res <- list()
  for(i in 1:length(dim)) {
    z <- data.frame(weight=res2,test.stat=res1[,dim[i]])
    z$p.value <- round(2*(1 -pnorm(abs(z$test.stat))),5)
    rownames(z) <- rownames(res1)
    z <- z[order(-z$test.stat),]
    z <- z[z$p.value<=max.pval,]
    res[[i]] <- z
    }
  names(res) <- paste('dim',dim,sep='.')
  return(res)
}
