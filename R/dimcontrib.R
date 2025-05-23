dimcontrib <- function(resmca,dim=c(1,2),best=TRUE) {
  
    type <- class(resmca)[1]
    
    if(type=="MCA") {
        resmca$var$weight <- colSums(dichotom(resmca$call$X[,resmca$call$quali]))
    }

    vardesc <- lapply(as.list(dim),function(x) data.frame(ctr=round(resmca$var$contrib[,x]*resmca$var$coord[,x]/abs(resmca$var$coord[,x]),2),weight=resmca$var$weight))#[-resmca$call$excl]))
    vardesc <- lapply(vardesc,function(x) x[order(x$ctr),])
    if(best==TRUE) vardesc <- lapply(vardesc,function(x) x[abs(x$ctr)>=100/nrow(resmca$var$coord),])
    inddesc <- lapply(as.list(dim),function(x) return(resmca$ind$contrib[,x]*resmca$ind$coord[,x]/abs(resmca$ind$coord[,x])))
    inddesc <- lapply(inddesc,function(x) x[order(x)])
    if(best==TRUE) inddesc <- lapply(inddesc,function(x) x[abs(x)>=100/nrow(resmca$ind$coord)])
    names(vardesc) <- paste('dim',dim,sep='.')
    names(inddesc) <- paste('dim',dim,sep='.')
    return(list(var=vardesc,ind=inddesc))
}
