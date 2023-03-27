ahc.plots <- function(ahc, distance = NULL, max.cl = 20, type = "dist") {
  
  if(type=="dist") {
    heights <- rev(sort(ahc$height, decreasing = TRUE)[1:max.cl])
    labs <- paste0((max.cl+1):2, ">", max.cl:1)
    labs <- factor(labs, levels = labs)
    graphics::barplot(heights ~ labs, xlab = "agregation", ylab = "distance between agregated clusters")
  }
  
  if(type=="inert") {
    if(is.null(distance)) stop("A distance matrix should be provided.")
    if (!requireNamespace("TraMineR", quietly = TRUE))
      stop("TraMineR package should be installed to use this type of plot")
    R2 <- sapply((max.cl+1):1, FUN = function(x) TraMineR::dissassoc(distance, stats::cutree(ahc, x), R = 1, square = TRUE)$stat$t0[3])
    labs <- max.cl:1
    labs <- factor(labs, levels = labs)
    graphics::barplot(R2[-1] ~ labs, xlab = "number of clusters", ylab = "% of explained inertia (pseudo-R2)")
  }
  
  if(type=="loss") {
    if(is.null(distance)) stop("A distance matrix should be provided.")
    if (!requireNamespace("TraMineR", quietly = TRUE))
      stop("TraMineR package should be installed to use this type of plot")
    R2 <- sapply((max.cl+1):1, FUN = function(x) TraMineR::dissassoc(distance, stats::cutree(ahc, x), R = 1, square = TRUE)$stat$t0[3])
    rel.loss <- (R2[1:max.cl]-R2[2:(max.cl+1)])/R2[1:max.cl]
    labs <- paste0((max.cl+1):2, ">", max.cl:1)
    labs <- factor(labs, levels = labs)
    graphics::barplot(rel.loss ~ labs, space= 0, xlab = "agregation", ylab = "relative loss of explained inertia (pseudo-R2)")
  }
  
}
