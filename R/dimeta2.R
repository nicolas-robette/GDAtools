dimeta2 <- function(resmca,vars,dim=c(1,2)) {
  
  type <- class(resmca)[1]
  
  vars <- as.data.frame(vars)
  n <- names(vars)
  eta2 <- matrix(nrow=ncol(vars), ncol=length(dim))
  sub <- rep(TRUE,times=nrow(resmca$ind$coord))
  if(type=='csMCA') sub <- resmca$call$subcloud
  if(type=='stMCA') if(resmca$call$input.mca=='csMCA') sub <- resmca$call$subcloud
  if(type=='multiMCA') if(class(resmca$my.mca[[1]])[1]=='csMCA') sub <- resmca$my.mca[[1]]$call$subcloud
  ww <- resmca$call$row.w[sub]
  if(type=='stMCA') ww <- resmca$call$fit$weights
  vars = vars[sub,]
  for(i in 1:length(dim)) {
    for(j in 1:ncol(vars)) {
      vars[,j] = factor(vars[,j])
      if(nlevels(vars[,j])>1) {
        eta2[j,i] <- summary(stats::lm(resmca$ind$coord[,dim[i]]~vars[,j],weights=ww))$r.squared 
      } else {
        eta2[j,i] <- NA
      }
    }}

  rownames(eta2) <- n
  colnames(eta2) <- paste("dim",dim, sep=".")
  return(round(100*eta2,1))
}
