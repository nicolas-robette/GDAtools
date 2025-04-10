ggadd_density <- function(p, resmca, var, cat=levels(var)[1], axes=c(1,2),
                          density="contour", col.contour="darkred", pal.area="viridis", alpha.area=0.2,
                          ellipse=FALSE){

  if("bcMCA" %in% attr(resmca,'class')) resmca = reshape_between(resmca)
  
  type <- attr(resmca,'class')[1]
  
  if(type=="stMCA") type <- resmca$call$input.mca
  if(type=="csMCA") var <- var[resmca$call$subcloud]
  if(type=="multiMCA") {
    if(class(resmca$my.mca[[1]])[1]=="csMCA") var <- var[resmca$my.mca[[1]]$call$subcloud]
  }
    
  df <- as.data.frame(resmca$ind$coord[,axes])
  names(df) <- c("dim.1","dim.2")
  df <- df[var==cat,]
  
  if(density=="contour") p <- p + ggplot2::stat_density_2d(data=df, ggplot2::aes(x=.data$dim.1, y=.data$dim.2), colour=col.contour, linewidth=0.2)
  
  if(density=="area") p <- p + ggplot2::stat_density_2d(data=df, ggplot2::aes(x=.data$dim.1, y=.data$dim.2, fill = ggplot2::after_stat(.data$level)), geom="polygon", alpha=alpha.area) +
                               ggplot2::scale_fill_continuous(type="viridis", option=pal.area) +
                               ggplot2::theme(legend.position="none")
    
  if(ellipse==TRUE) p <- ggadd_kellipses(p=p, resmca=resmca, var=var, sel=which(levels(var)==cat), axes=axes, size=0.3, label=FALSE, points=FALSE, legend="none")
  
  return(p)
}
