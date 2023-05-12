supvar <- function(resmca,var) {
  
  type <- attr(resmca,'class')[1]
  
  if(type %in% c("MCA","stMCA","multiMCA")) eigen <- resmca$eig[,"eigenvalue"]
  if(type %in% c("speMCA","csMCA")) eigen <- resmca$eig$eigen
  
  if(type=="stMCA") {
    if(resmca$call$input.mca %in% c("MCA","speMCA","csMCA")) type <- resmca$call$input.mca
  }
  
  if(type=="multiMCA") {
    classe_afm <- class(resmca$my.mca[[1]])[1]
    if(classe_afm %in% c("MCA","speMCA","csMCA")) type <- classe_afm
    if(classe_afm=="csMCA") {
      resmca$call$row.w <- resmca$my.mca[[1]]$call$row.w
      resmca$call$subcloud <- resmca$my.mca[[1]]$call$subcloud
    }
  }
  
  if(type %in% c("MCA","speMCA")) {
    wt <- resmca$call$row.w
    v <- factor(var)
    n <- sum(wt)
    FK <- colSums(wt*(dichotom(as.data.frame(v),out='numeric')))/n
    ind <- resmca$ind$coord
    coord <- aggregate(wt*ind,list(v),sum)[,-1]/n/FK
    vrc <- aggregate(wt*ind*ind,list(v),sum)[,-1]/n/FK-coord*coord
    for(i in 1:resmca$call$ncp) coord[,i] <- coord[,i]/resmca$svd$vs[i]
    cos2 <- coord*coord/((1/FK)-1)
    weight=n*FK
  }
  
  if(type=="csMCA") {
    wt <- resmca$call$row.w
    n <- sum(wt)
    v <- factor(var)
    FK <- colSums(wt*(dichotom(as.data.frame(v),out='numeric')))/n
    wt <- wt[resmca$call$subcloud]
    n.w <- sum(wt)
    v <- factor(var[resmca$call$subcloud])
    fK <- colSums(wt*(dichotom(as.data.frame(v),out='numeric')))/n.w
    ind <- resmca$ind$coord
    coord <- aggregate(wt*ind,list(v),sum)[-1]/n.w/fK
    vrc <- aggregate(wt*ind*ind,list(v),sum)[,-1]/n.w/fK-coord*coord
    for(i in 1:resmca$call$ncp) coord[,i] <- coord[,i]/resmca$svd$vs[i]
    cos2 <- coord*coord*FK*FK/fK/(1-fK)
    weight <- length(wt)*fK
  }
  
  names(weight) <- levels(v)
  rownames(coord) <- levels(v)
  rownames(cos2) <- levels(v)
  wi <- apply(vrc,2,weighted.mean,w=weight)
  be <- eigen[1:resmca$call$ncp]-wi
  eta2 <- be/eigen[1:resmca$call$ncp]
  vrc <- rbind(vrc,wi,be,eigen[1:resmca$call$ncp],eta2)
  vrc <- round(vrc,6)
  rownames(vrc) <- c(levels(v),'within','between','total','eta2')
  coord <- round(coord,6)
  typic <- sqrt(cos2)*sqrt(length(v)-1)
  typic <- (((abs(coord)+coord)/coord)-1)*typic
  pval <- 2*(1 -pnorm(abs(as.matrix(typic))))
  cor <- sapply(as.data.frame(ind), function(x) descriptio::assoc.catcont(v,x,wt,nperm=NULL,na.rm.cat=TRUE)$cor)
  list(weight=round(weight,1),coord=coord,cos2=round(cos2,6),var=round(vrc,6),typic=round(typic,6),pval=round(pval,6),cor=cor)
}



varsup <- function(resmca,var) {
  
  warning("varsup function is softly deprecated. Please use supvar function instead")
  
  return(supvar(resmca,var))
  
  }
