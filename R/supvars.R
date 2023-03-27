supvars <- function(resmca, vars) {
  if(any(sapply(vars, FUN = function(x) !is.factor(x)))) stop("variables in data should all be factors")
  tmp <- lapply(vars, function(x) supvar(resmca, x))
  res <- list()
  res$weight <- unlist(lapply(tmp, function(x) x$weight))
  res$coord <- do.call("rbind.data.frame", lapply(tmp, function(x) x$coord))
  res$cos2 <- do.call("rbind.data.frame", lapply(tmp, function(x) x$cos2))
  res$var <- lapply(tmp, function(x) x$var)
  res$typic <- do.call("rbind.data.frame", lapply(tmp, function(x) x$typic))
  res$pval <- do.call("rbind.data.frame", lapply(tmp, function(x) x$pval))
  res$cor <- do.call("rbind.data.frame", lapply(tmp, function(x) x$cor))
  return(res)
}



varsups <- function(resmca, vars) {

  warning("varsups function is softly deprecated. Please use supvars function instead")
  
  return(supvars(resmca,vars))
}

