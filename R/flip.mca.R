flip.mca <- function(resmca, dim=1) {
  
  type <- attr(resmca,'class')[1]
  
  resmca$ind$coord[,dim] <- -resmca$ind$coord[,dim]
  resmca$var$coord[,dim] <- -resmca$var$coord[,dim]
  
  if(type %in% c("MCA","speMCA","csMCA","stMCA")) resmca$var$v.test[,dim] <- -resmca$var$v.test[,dim]

  if(type=="multiMCA") {
    resmca$var$cor[,dim] <- -resmca$var$cor[,dim]
    for(i in 1:length(resmca$VAR)) {
      resmca$VAR[[i]]$coord[,dim] <- -resmca$VAR[[i]]$coord[,dim]
      resmca$VAR[[i]]$v.test[,dim] <- -resmca$VAR[[i]]$v.test[,dim]
    }
  }
  
  if(type=="MCA") {
    resmca$quanti.sup$coord[,dim] <- -resmca$quanti.sup$coord[,dim]  
    resmca$quali.sup$coord[,dim] <- -resmca$quali.sup$coord[,dim]
    resmca$quali.sup$v.test[,dim] <- -resmca$quali.sup$v.test[,dim]
  }
  
  if(type == "bcMCA") {
    resmca$row$coord[,dim] <- -resmca$row$coord[,dim]
    resmca$col$coord[,dim] <- -resmca$col$coord[,dim]
    resmca$row.sup$coord[,dim] <- -resmca$row.sup$coord[,dim]
  }
  
  return(resmca)
  
}
