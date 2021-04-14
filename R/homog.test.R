homog.test <- function(resmca,var) {
  vs <- varsup(resmca,var)
  N <- sum(vs$weight)
  a <- sqrt(1/outer(1/vs$weight,1/vs$weight,"+"))
  ncp <- resmca$call$ncp
  res <- vector("list",ncp)
  for(i in 1:ncp) {
    temp <- abs(outer(vs$coord[,i],vs$coord[,i],"-"))/sqrt(resmca$eig[[1]][i])
    test.stat <- a*sqrt((N-1)/N)*temp
    res[[i]]$test.stat <- test.stat
    res[[i]]$p.values <- 2*(1 -pnorm(test.stat))
  }
  names(res) <- paste('dim',1:ncp,sep='.')
  return(res)
}
