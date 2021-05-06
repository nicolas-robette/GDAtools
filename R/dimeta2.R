dimeta2 <- function(resmca,vars,dim=c(1,2)) {
  n=names(vars)
  eta2 <- matrix(nrow=ncol(vars),ncol=length(dim))
  sub <- rep(TRUE,times=nrow(resmca$ind$coord))
  if(class(resmca)[1]=='csMCA') sub <- resmca$call$subcloud
  if(class(resmca)[1]=='stMCA') if(resmca$call$input.mca=='csMCA') sub <- resmca$call$subcloud
  if(class(resmca)[1]=='multiMCA') if(class(resmca$my.mca[[1]])[1]=='csMCA') sub <- resmca$my.mca[[1]]$call$subcloud
  ww <- resmca$call$row.w[sub]
  if(class(resmca)[1]=='stMCA') ww <- resmca$call$fit$weights
  for(i in 1:length(dim)) {
    for(j in 1:ncol(vars)) eta2[j,i] <- summary(lm(resmca$ind$coord[,dim[i]]~vars[,j][sub],weights=ww))$r.squared
   }
  rownames(eta2) <- n
  colnames(eta2) <- paste("dim",dim, sep=".")
  return(round(100*eta2,1))
  }