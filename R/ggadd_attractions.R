ggadd_attractions <- function(p, resmca, axes=c(1,2), measure="phi", min.asso=0.3,
                              col.segment="lightgray", col.text="black", text.size=3) {
  
  type <- attr(resmca,'class')[1]
  if(type=="MCA") {
    resmca$call$X <- resmca$call$X[,resmca$call$quali]
    rownames(resmca$var$coord) <- names(dichotom(resmca$call$X))
  }
  if(type=="stMCA") {
    for(i in 1:ncol(resmca$call$X)) levels(resmca$call$X[,i]) <- gsub(names(resmca$call$X)[i],"",gsub("_","",levels(resmca$call$X[,i])))
    type <- resmca$call$input.mca
  }
  if(type=="multiMCA") {
    listX <- lapply(resmca$my.mca, function(x) x$call$X)
    nk <- sapply(listX, function(x) ncol(dichotom(x)))
    listexcl <- lapply(resmca$my.mca, function(x) x$call$excl)
    for(i in 2:length(listexcl)) listexcl[[i]] <- listexcl[[i]] + cumsum(nk)[i-1]
    resmca$call$excl <- unlist(listexcl)
    resmca$call$X <- do.call("cbind.data.frame", listX)
    resmca$var$coord <- do.call("rbind.data.frame", lapply(resmca$VAR, function(x) x$coord))
    rownames(resmca$var$coord) <- names(dichotom(resmca$call$X))[-resmca$call$excl]
    type <- class(resmca$my.mca[[1]])[1]
    if(type=="csMCA") {
      resmca$call$subcloud <- resmca$my.mca[[1]]$call$subcloud
      resmca$call$row.w <- resmca$my.mca[[1]]$call$row.w
    }
  }
  
  # if(type %in% c("MCA","speMCA")) w <- resmca$call$row.w
  # if(type=="csMCA") w <- resmca$call$row.w[resmca$call$subcloud]
  
  paires <- as.data.frame(t(combn(names(resmca$call$X),2)),stringsAsFactors=FALSE)
  names(paires) <- c("v1","v2")
  l <- list()
  for(i in 1:nrow(paires)) {
    if(measure=="phi") {
      if(type %in% c("MCA","speMCA")) l[[i]] <- as.data.frame(phi.table(resmca$call$X[,paires[i,1]],resmca$call$X[,paires[i,2]],weights=resmca$call$row.w))
      if(type=="csMCA") l[[i]] <- as.data.frame(phi.table(resmca$call$X[resmca$call$subcloud,paires[i,1]],resmca$call$X[resmca$call$subcloud,paires[i,2]],weights=resmca$call$row.w[resmca$call$subcloud]))
    }
    if(measure=="pem") {
      if(type %in% c("MCA","speMCA")) l[[i]] <- as.data.frame(pem(resmca$call$X[,paires[i,1]],resmca$call$X[,paires[i,2]],weights=resmca$call$row.w)$peml/100)
      if(type=="csMCA") l[[i]] <- as.data.frame(pem(resmca$call$X[resmca$call$subcloud,paires[i,1]],resmca$call$X[resmca$call$subcloud,paires[i,2]],weights=resmca$call$row.w[resmca$call$subcloud])$peml/100)
    }
    l[[i]]$v1 <- paires$v1[i]
    l[[i]]$v2 <- paires$v2[i]
  }
  assoc <- do.call("rbind.data.frame",l)
  assoc$Var1 <- paste(assoc$v1,assoc$Var1,sep=".")
  assoc$Var2 <- paste(assoc$v2,assoc$Var2,sep=".")
  assoc <- assoc[,c("Var1","Var2","Freq")]
  noms.cat <- names(dichotom(resmca$call$X))
  if(type %in% c("speMCA","csMCA")) noms.cat <- noms.cat[-resmca$call$excl]
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
  
  p <- p + ggplot2::geom_segment(data=assoc, ggplot2::aes(x=.data$x, y=.data$y, xend=.data$xend, yend=.data$yend), col=col.segment) +
           ggplot2::geom_text(data=vars, ggplot2::aes(x=.data$x, y=.data$y, label=.data$Var), col=col.text, size=text.size)
  
  return(p)
}