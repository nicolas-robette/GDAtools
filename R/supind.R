supind <- function(resmca,supdata) {
    supdata <- data.frame(supdata)
    type <- attr(resmca, "class")[1]
    if(type %in% c("MCA", "stMCA", "multiMCA", "PCA")) eigen <- resmca$eig[,"eigenvalue"]
    if(type %in% c("speMCA", "csMCA")) eigen <- resmca$eig$eigen
    z <- as.matrix(dichotom(supdata, out = "numeric"))
    if(type %in% c("speMCA", "csMCA")) z <- z[,-resmca$call$excl]
    Q <- ncol(supdata)
    delta <- 1/sqrt(eigen[1:resmca$call$ncp])
    vcoord <- resmca$var$coord
    coord <- (1/Q)*z%*%vcoord
    coord <- sweep(coord,2,delta,'*')
    GM2 <- rowSums(coord^2)
    cos2 <- sweep(coord^2,1,GM2,'/')
    cos2 <- round(cos2,6)
    rownames(coord) <- rownames(supdata)
    rownames(cos2) <- rownames(supdata)
    return(list(coord = coord, cos2 = cos2))
}



indsup <- function(resmca,supdata) {
  
  warning("indsup function is softly deprecated. Please use supind function instead")

  return(supind(resmca,supdata))
  
}