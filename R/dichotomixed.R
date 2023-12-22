dichotomixed <- function(data, out = "numeric") {
  res <- list()
  for(i in 1:ncol(data)) {
    if(is.factor(data[,i])) { 
      res[[i]] <- as.data.frame(dichotom(data[,i], out = out))
      names(res[[i]]) <- paste(colnames(data)[i], levels(data[,i]), sep = ".")
    } else {
      res[[i]] <- as.data.frame(data[,i])
      colnames(res[[i]]) <- colnames(data)[i]
    }
  }
  res <- as.data.frame(res)
  return(res)
}
