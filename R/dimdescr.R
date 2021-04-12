dimdescr <- function(resmca,ncp=1:3,min.cor=NULL,nperm=100,distrib="asympt") {
   res <- list()
   X <- resmca$call$X
   classe <- class(resmca)[1]
   if(classe=='stMCA') classe=resmca$call$input.mca
   for(i in ncp) {
      if(classe %in% c('MCA','speMCA')) temp <- condesc(resmca$ind$coord[,i],X,weights=resmca$call$row.w,min.cor=min.cor,nperm=nperm,distrib=distrib)
      if(classe == 'csMCA') temp <- condesc(resmca$ind$coord[,i],X[resmca$call$subcloud,],weights=resmca$call$row.w,min.cor=min.cor,nperm=nperm,distrib=distrib)
      rownames(temp) <- NULL
      res[[i]] <- temp
      }
   names(res) <- paste('dim',ncp,sep='.')
   return(res)
}
