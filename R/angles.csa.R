angles.csa <- function(rescsa, resmca) {

  csa.coord <- rescsa$ind$coord
  mca.coord <- resmca$ind$coord[rescsa$call$subcloud,]
  
  cosines <- cor(csa.coord,mca.coord)
  rownames(cosines) <- paste("csa", gsub(".","",rownames(cosines),fixed=TRUE), sep=".")
  colnames(cosines) <- paste("mca", gsub(".","",colnames(cosines),fixed=TRUE), sep=".")

  for(i in 1:(ncol(mca.coord)-1)) {
    for(j in (i+1):ncol(mca.coord)) {
      cosines <- cbind(cosines, sqrt(cosines[,i]^2 + cosines[,j]^2))
      colnames(cosines)[ncol(cosines)] <- paste0("mca.dim",i,"&",j)
    }
  }

  angles <- acos(abs(cosines))/pi * 180
    
  return(list(cosines = round(cosines,3), angles = round(angles,1)))
}
