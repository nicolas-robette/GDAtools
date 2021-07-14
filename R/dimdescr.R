dimdescr <- function(resmca,vars=NULL,dim=c(1,2),min.cor=NULL,nperm=100,distrib="asympt") {
   classe <- class(resmca)[1]
   if(classe=="MCA") resmca$call$X <- resmca$call$X[,resmca$call$quali]
   if(classe=="multiMCA") {
      listX <- lapply(resmca$my.mca, function(x) x$call$X)
      resmca$call$X <- do.call("cbind.data.frame", listX)
      classe <- class(resmca$my.mca[[1]])[1]
      if(classe=="csMCA") {
         resmca$call$subcloud <- resmca$my.mca[[1]]$call$subcloud
         resmca$call$row.w <- resmca$my.mca[[1]]$call$row.w
      }
   }
   res <- list()
   if(is.null(vars)) X <- resmca$call$X
   if(!is.null(vars)) X <- as.data.frame(vars)
   if(classe=='stMCA') classe=resmca$call$input.mca
   for(i in 1:length(dim)) {
      if(classe %in% c('MCA','speMCA')) temp <- condesc(resmca$ind$coord[,dim[i]],X,weights=resmca$call$row.w,min.cor=min.cor,nperm=nperm,distrib=distrib,dec=c(3,3,3,3),robust=FALSE)
      if(classe == 'csMCA') temp <- condesc(resmca$ind$coord[,dim[i]],X[resmca$call$subcloud,],weights=resmca$call$row.w[resmca$call$subcloud],min.cor=min.cor,nperm=nperm,distrib=distrib,dec=c(3,3,3,3),robust=FALSE)
      rownames(temp) <- NULL
      res[[i]] <- temp
      }
   names(res) <- paste('dim', dim, sep='.')
   return(res)
}
