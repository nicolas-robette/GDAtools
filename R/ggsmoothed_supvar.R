ggsmoothed_supvar <- function(resmca, var, cat, axes = c(1,2), 
                              center = FALSE, scale = FALSE,
                              nc = c(20, 20), power = 2,
                              limits = NULL, pal = "RdBu") {
  
  if (!requireNamespace("sf", quietly = TRUE))
    stop("sf package should be installed to use this function")
  
  type <- attr(resmca,'class')[1]
  
  if(type=="stMCA") type <- resmca$call$input.mca
  if(type=="csMCA") var <- var[resmca$call$subcloud]
  if(type=="multiMCA") {
    if(class(resmca$my.mca[[1]])[1]=="csMCA") var <- var[resmca$my.mca[[1]]$call$subcloud]
  }

  if(type %in% c("MCA","speMCA","csMCA")) {
    rate1 <- modif.rate(resmca)$modif$mrate[axes[1]]
    rate2 <- modif.rate(resmca)$modif$mrate[axes[2]]
  } else if(type %in% c("stMCA","multiMCA","PCA")) {
    rate1 <- modif.rate(resmca)$raw$rate[axes[1]]
    rate2 <- modif.rate(resmca)$raw$rate[axes[2]]
  } else if(type == "bcMCA") {
    rate1 <- resmca$eig$rate[axes[1]]
    rate2 <- resmca$eig$rate[axes[2]]
  }
  
  # prepare data  
  df <- as.data.frame(resmca$ind$coord[,axes])
  names(df) <- c("x","y")
  if(is.numeric(var)) df$z <- var
  if(is.factor(var)) df$z <- as.numeric(var==cat)
  if(center) df$z <- df$z - mean(df$z)
  if(scale) df$z <- df$z / stats::sd(df$z)
  df$x <- jitter(df$x)
  df$y <- jitter(df$y)
  
  # make grid
  points <- sf::st_as_sf(df, coords = c("x", "y"))
  hull <- sf::st_union(points)
  hull <- sf::st_convex_hull(hull)
  hull <- sf::st_buffer(hull, dist = 0.1)
  grid0 <- sf::st_make_grid(points, n = nc, square = FALSE)
  grid <- sf::st_intersection(grid0, hull)
  grid <- sf::st_as_sf(grid)

  # compute distances
  xyp <- sf::st_coordinates(points)
  xyg <- sf::st_coordinates(sf::st_centroid(grid))
  xy <- rbind(xyg, xyp)
  dis <- as.matrix(stats::dist(xy, method = "euclidean"))
  dis <- dis[1:nrow(xyg), (nrow(xyg)+1):ncol(dis)]

  # compute idw
  D <- 1/(dis^power)
  sumD <- apply(D, 1, sum)
  idw <- D %*% df$z / sumD
  grid$z <- idw[,1]

  # plot
  if(is.null(limits)) {
    if(center) {
      lim <- max(abs(grid$z))
      limits <- c(-lim, lim)*1.01
    }
  }

  p <- 
    ggplot2::ggplot() +
      ggplot2::geom_sf(data = grid, ggplot2::aes(geometry = .data$x, fill = .data$z), col = NA) +
      ggplot2::scale_fill_distiller(type = "div", palette = pal, limits = limits, name = "")
  
  p + 
    ggplot2::geom_hline(yintercept = 0, colour = "darkgrey", linewidth = .1) +
    ggplot2::geom_vline(xintercept = 0, colour = "darkgrey", linewidth = .1) +
    ggplot2::xlab(paste0("dim ", axes[1], " (", round(rate1,1), " %)")) +
    ggplot2::ylab(paste0("dim ", axes[2], " (", round(rate2,1), " %)")) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())
 
}
