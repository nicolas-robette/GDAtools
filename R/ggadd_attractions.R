ggadd_attractions <- function(p, resmca, axes=c(1,2), measure="phi", min.asso=0.3,
                              col.segment="lightgray", col.text="black", text.size=3) {
  
  paires <- as.data.frame(t(combn(names(resmca$call$X),2)),stringsAsFactors=FALSE)
  names(paires) <- c("v1","v2")
  l <- list()
  for(i in 1:nrow(paires)) {
    if(measure=="phi") l[[i]] <- as.data.frame(phi.table(resmca$call$X[,paires[i,1]],resmca$call$X[,paires[i,2]]))
    if(measure=="pem") l[[i]] <- as.data.frame(pem(resmca$call$X[,paires[i,1]],resmca$call$X[,paires[i,2]])$peml/100)
  }
  assoc <- cbind(do.call("rbind.data.frame",l),paires)
  assoc$Var1 <- paste(assoc$v1,assoc$Var1,sep=".")
  assoc$Var2 <- paste(assoc$v2,assoc$Var2,sep=".")
  assoc <- assoc[,c("Var1","Var2","Freq")]
  noms.cat <- names(resmca$call$marge.col)
  assoc <- assoc[assoc$Var1 %in% noms.cat & assoc$Var2 %in% noms.cat,]
  assoc <- assoc[assoc$Freq>=min.asso,]
  assoc$x <- resmca$var$coord[assoc$Var1,axes[1]]
  assoc$y <- resmca$var$coord[assoc$Var1,axes[2]]
  assoc$xend <- resmca$var$coord[assoc$Var2,axes[1]]
  assoc$yend <- resmca$var$coord[assoc$Var2,axes[2]]
  
  t1 <- assoc[,c("Var1","x","y")]
  t2 <- assoc[,c("Var2","xend","yend")]
  names(t1) <- names(t2) <- c("Var","x","y")
  vars <- unique(rbind(t1,t2))
  
  p <- p + ggplot2::geom_segment(data=assoc[assoc$Freq>=min.asso,], aes(x=.data$x, y=.data$y, xend=.data$xend, yend=.data$yend), col=col.segment) +
           ggplot2::geom_text(data=vars, ggplot2::aes(x=.data$x, y=.data$y, label=.data$Var), col=col.text, size=text.size)
  
  return(p)
}