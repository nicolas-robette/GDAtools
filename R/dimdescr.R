dimdescr <- function(resmca, vars = NULL, dim = c(1,2), 
                     limit = NULL, correlation = "pearson",
                     na.rm.cat = FALSE, na.value.cat = "NA", na.rm.cont = FALSE,
                     nperm = NULL, distrib = "asympt") {
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
      if(classe %in% c('MCA','speMCA')) temp <- descriptio::condesc(resmca$ind$coord[,dim[i]], X, weights = resmca$call$row.w,
                                                                    limit = limit, correlation = correlation,
                                                                    na.rm.cat = na.rm.cat, na.value.cat = na.value.cat, na.rm.cont = na.rm.cont,
                                                                    nperm = nperm, distrib = distrib, dec = c(3,3,3,3), robust = FALSE)
      if(classe == 'csMCA') temp <- descriptio::condesc(resmca$ind$coord[,dim[i]], X[resmca$call$subcloud,], weights = resmca$call$row.w[resmca$call$subcloud],
                                                        limit = limit, correlation = correlation,
                                                        na.rm.cat = na.rm.cat, na.value.cat = na.value.cat, na.rm.cont = na.rm.cont,                                                        nperm = nperm, distrib = distrib, dec = c(3,3,3,3), robust = FALSE)
      rownames(temp) <- NULL
      res[[i]] <- temp
      }
   names(res) <- paste('dim', dim, sep='.')
   return(res)
}
