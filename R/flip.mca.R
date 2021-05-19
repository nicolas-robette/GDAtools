flip.mca <- function(resmca, dim=1) {
  
  newmca <- resmca
  type <- attr(resmca,'class')[1]
  
  newmca$ind$coord[,dim] <- -newmca$ind$coord[,dim]
  newmca$var$coord[,dim] <- -newmca$var$coord[,dim]
  
  if(type %in% c("MCA","speMCA","csMCA","stMCA")) newmca$var$v.test[,dim] <- -newmca$var$v.test[,dim]

  if(type=="multiMCA") {
    newmca$var$cor[,dim] <- -newmca$var$cor[,dim]
    for(i in 1:length(newmca$VAR)) {
      newmca$VAR[[i]]$coord[,dim] <- -newmca$VAR[[i]]$coord[,dim]
      newmca$VAR[[i]]$v.test[,dim] <- -newmca$VAR[[i]]$v.test[,dim]
    }
  }
  
  if(type=="MCA") {
    newmca$quanti.sup$coord[,dim] <- -newmca$quanti.sup$coord[,dim]  
    newmca$quali.sup$coord[,dim] <- -newmca$quali.sup$coord[,dim]
    newmca$quali.sup$v.test[,dim] <- -newmca$quali.sup$v.test[,dim]
  }
  
  return(newmca)
  
}
