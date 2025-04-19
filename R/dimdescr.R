dimdescr <- function(resmca, vars = NULL, dim = c(1,2), 
                     limit = NULL, correlation = "pearson",
                     na.rm.cat = FALSE, na.value.cat = "NA", na.rm.cont = FALSE,
                     nperm = NULL, distrib = "asympt",
                     shortlabs = TRUE) {

   type <- class(resmca)[1]
   if(type=="MCA") resmca$call$X <- resmca$call$X[,resmca$call$quali]
   if(type=="multiMCA") {
      listX <- lapply(resmca$my.mca, function(x) x$call$X)
      resmca$call$X <- do.call("cbind.data.frame", listX)
      type <- class(resmca$my.mca[[1]])[1]
      if(type=="csMCA") {
         resmca$call$subcloud <- resmca$my.mca[[1]]$call$subcloud
         resmca$call$row.w <- resmca$my.mca[[1]]$call$row.w
      }
   }
   res <- list()
   if(is.null(vars)) X <- resmca$call$X
   if(!is.null(vars)) X <- as.data.frame(vars)
   if(type=='stMCA') type=resmca$call$input.mca
   for(i in 1:length(dim)) {
      if(type %in% c('MCA','speMCA','bcMCA')) temp <- descriptio::condesc(resmca$ind$coord[,dim[i]], X, weights = resmca$call$row.w,
                                                                    limit = limit, correlation = correlation,
                                                                    na.rm.cat = na.rm.cat, na.value.cat = na.value.cat, na.rm.cont = na.rm.cont,
                                                                    nperm = nperm, distrib = distrib, digits = 3, robust = FALSE)
      if(type == 'csMCA') temp <- descriptio::condesc(resmca$ind$coord[,dim[i]], X[resmca$call$subcloud,], weights = resmca$call$row.w[resmca$call$subcloud],
                                                        limit = limit, correlation = correlation,
                                                        na.rm.cat = na.rm.cat, na.value.cat = na.value.cat, na.rm.cont = na.rm.cont,
                                                        nperm = nperm, distrib = distrib, digits = 3, robust = FALSE)
      rownames(temp$variables) <- NULL
      rownames(temp$categories) <- NULL
      temp$categories$overall.mean <- NULL
      if(type %in% c("speMCA","csMCA","bcMCA") & is.null(vars)) temp$categories <- temp$categories[!(temp$categories$categories %in% resmca$call$excl.char),]
      if(shortlabs) {
        labs <- c("categories", "avg.coord.in.cat", "sd.coord.in.cat", "sd.coord.in.dim", "cor")
        if(!is.null(nperm)) labs <- c(labs, "pval")
        colnames(temp$categories) <- labs
      } else {
        labs <- c("Categories",
                  "Average coordinate of category points",
                  "Standard deviation of the coordinates of category points",
                  "Standard deviation of all points",
                  "Point biserial correlation")
        if(!is.null(nperm)) labs <- c(labs, "Permutation p-value")
        colnames(temp$categories) <- labs
      }
      res[[i]] <- temp
      }
   names(res) <- paste('dim', dim, sep='.')
   return(res)
}
