ggcloud_indiv <- function(resmca, type = "i", points = "all", axes = c(1,2),
                          col = "dodgerblue4", point.size = 0.5, alpha = 0.6, repel = FALSE, text.size = 2,
                          density = NULL, col.contour = "darkred", hex.bins = 50, hex.pal = "viridis") {

  dim1 <- axes[1]
  dim2 <- axes[2]
  ni <- nrow(resmca$ind$coord)
  if(points=='all') condi <- 1:ni
  if (points=='besth') condi <- resmca$ind$contrib[,dim1] >= 100/ni
  if (points=='bestv') condi <- resmca$ind$contrib[,dim2] >= 100/ni
  if (points=='besthv') condi <- resmca$ind$contrib[,dim1] >= 100/ni | resmca$ind$contrib[,dim2] >= 100/ni
  if (points=='best') condi <- planecontrib(resmca, axes)$ind$ctr >= 100/ni
  icoord <- as.data.frame(resmca$ind$coord[condi,axes])
  names(icoord) <- c('axeX','axeY')
  icoord$labs <- rownames(icoord)
  if(length(col)==ni) col <- col[condi]
  if(is.factor(col)) icoord$col <- col
  
  p <- ggplot2::ggplot(icoord, ggplot2::aes(x = .data$axeX, y = .data$axeY))
  
  if(type=='i' & is.character(col)) {
    p <- p + 
         ggplot2::geom_point(size = point.size, alpha = alpha, colour = col) + 
         ggplot2::guides(colour = "none")
  
  } else if(type=='i' & is.factor(col)) {
    p <- p +
         ggplot2::geom_point(ggplot2::aes(colour = .data$col),
                             size = point.size, alpha = alpha) +
         ggplot2::guides(colour = ggplot2::guide_legend(title = ""))

  } else if(type=='inames' & is.character(col)) {
    p <- p + ggplot2::guides(colour = "none")
    if(repel==TRUE) { 
      p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = .data$labs),
                                        size = text.size, alpha = alpha, colour = col)
    } else { 
      p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$labs),
                                  size = text.size, alpha = alpha, colour = col) }
  
  } else if(type=='inames' & is.factor(col)) { 
    p <- p + ggplot2::guides(colour = ggplot2::guide_legend(title = ""))
    if(repel==TRUE) { 
      p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = .data$labs, colour = .data$col),
                                        size = text.size, alpha = alpha)
    } else {
      p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$labs, colour = .data$col),
                                  size = text.size, alpha = alpha) }
    }
  
  if(!is.null(density)) {
    if(density=="contour") p <- p + ggplot2::stat_density_2d(colour = col.contour, linewidth = 0.2)
    if(density=="hex") p <- p + ggplot2::geom_hex(bins = hex.bins) +
                                ggplot2::scale_fill_continuous(type = "viridis", option = hex.pal)
  }

  type <- attr(resmca,'class')[1]
  if(type %in% c("MCA","speMCA","csMCA")) {
    rate1 <- modif.rate(resmca)$modif$mrate[dim1]
    rate2 <- modif.rate(resmca)$modif$mrate[dim2]
  }
  if(type %in% c("stMCA","multiMCA","PCA")) {
    rate1 <- modif.rate(resmca)$raw$rate[dim1]
    rate2 <- modif.rate(resmca)$raw$rate[dim2]
  }
  if(type == "bcMCA") {
    rate1 <- resmca$eig$rate[dim1]
    rate2 <- resmca$eig$rate[dim2]
  }
  
  p + ggplot2::geom_hline(yintercept = 0, colour = "darkgrey", linewidth = .1) +
      ggplot2::geom_vline(xintercept = 0, colour = "darkgrey", linewidth = .1) +
      ggplot2::xlab(paste0("dim ", dim1, " (", round(rate1,1), " %)")) +
      ggplot2::ylab(paste0("dim ", dim2, " (", round(rate2,1), " %)")) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     legend.position="none")
}
