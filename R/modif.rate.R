modif.rate <- function(resmca) {
      type <- attr(resmca,'class')[1]
      if(type=="MCA") {
         Q <- length(resmca$call$quali)
         eigen <- resmca$eig[,"eigenvalue"]
      }
      if(type %in% c("speMCA","csMCA")) {
         Q <- ncol(resmca$call$X)
         eigen <- resmca$eig$eigen
      }
      if(type %in% c("stMCA","multiMCA")) {
         Q <- ncol(resmca$call$X)
         eigen <- resmca$eig[,"eigenvalue"]
      }
      rate <- eigen/sum(eigen)*100
      cum.rate <- cumsum(rate)
      seuil <- 1/Q
      e <- eigen[eigen>=seuil]
      pseudo <- (Q/(Q-1)*(e-seuil))^2
      mrate <- pseudo/sum(pseudo)*100
      cum.mrate <- cumsum(mrate)
      return(list(raw=data.frame(eigen,rate,cum.rate), modif=data.frame(mrate,cum.mrate)))
}
