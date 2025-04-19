quadrant <- function(resmca, dim = c(1,2)) {
  v1 <- factor(sign(resmca$ind$coord[,dim[[1]]]), labels = c("left","right"))
  v2 <- factor(sign(resmca$ind$coord[,dim[[2]]]), labels = c("lower","upper"))
  res <- interaction(v2,v1,sep="_")
  return(res)
}
