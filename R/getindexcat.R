getindexcat <- function(data) {
  for(i in 1:ncol(data)) data[,i] <- factor(data[,i])
  colnames(dichotom(data))
  }
