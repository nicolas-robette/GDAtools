homog.test <- function(resmca,var,dim=c(1,2)) {

  # type <- attr(resmca,'class')[1]
  # if(type %in% c("MCA","stMCA","multiMCA")) eigen <- resmca$eig[,"eigenvalue"]
  # if(type %in% c("speMCA","csMCA")) eigen <- resmca$eig$eigen

  vs <- supvar(resmca,var)
  N <- sum(vs$weight)
  a <- sqrt(1/outer(1/vs$weight,1/vs$weight,"+"))
  res <- vector("list",length(dim))
  for(i in 1:length(dim)) {
    temp <- abs(outer(vs$coord[,dim[i]],vs$coord[,dim[i]],"-"))
    test.stat <- a*sqrt((N-1)/N)*temp
    res[[i]]$test.stat <- test.stat
    res[[i]]$p.values <- 2*(1 -pnorm(test.stat))
  }
  names(res) <- paste('dim',dim,sep='.')
  return(res)
}
