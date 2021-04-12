# p <- ggcloud_indiv(ACM, col="lightgray")
# ggadd_density(p, ACM, CRIT$Ctelerama, "5")
# ggadd_density(p, ACM, CRIT$Ctelerama, "5", ellipse=TRUE, col.ellipse="darkgreen")
# ggadd_density(p, ACM, CRIT$Ctelerama, "5", density="area")
# ggadd_density(p, ACM, CRIT$Ctelerama, "5", density="area", pal.area="magma", alpha.area=0.1)
# ggadd_density(p, ACM, CRIT$Ctelerama, "5", density="area", ellipse=TRUE)

ggadd_density <- function(p, resmca, var, cat=levels(var)[1], axes=c(1,2),
                          density="contour", col.contour="darkred", pal.area="viridis", alpha.area=0.2,
                          ellipse=FALSE, col.ellipse="black"){
  
  df <- as.data.frame(resmca$ind$coord[,axes])
  names(df) <- c("dim.1","dim.2")
  df <- df[var==cat,]
  
  if(density=="contour") p <- p + ggplot2::stat_density_2d(data=df, ggplot2::aes(x=.data$dim.1, y=.data$dim.2), colour=col.contour, size=0.2)
  
  if(density=="area") p <- p + ggplot2::stat_density_2d(data=df, ggplot2::aes(x=.data$dim.1, y=.data$dim.2, fill = .data$..level..), geom="polygon", alpha=alpha.area) +
                               ggplot2::scale_fill_continuous(type="viridis", option=pal.area) +
                               ggplot2::theme(legend.position="none")
    
  if(ellipse==TRUE) p <- p + ggplot2::stat_ellipse(data=df, ggplot2::aes(x=.data$dim.1, y=.data$dim.2), colour=col.ellipse, level=0.86, type='norm', size=.2)
    
  return(p)
}