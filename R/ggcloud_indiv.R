ggcloud_indiv <- function(resmca, type='i', points='all', axes=1:2, col='dodgerblue4', palette='Set2', size=0.5, alpha=0.6, repel=FALSE,
                           density=NULL, col.contour="darkred", hex.bins=50, hex.pal="viridis") {

  dim1 <- axes[1]
  dim2 <- axes[2]
  ni <- nrow(resmca$ind$coord)
  if(points=='all') condi <- 1:ni
  if (points=='besth') condi <- resmca$ind$contrib[,dim1]>=100/ni
  if (points=='bestv') condi <- resmca$ind$contrib[,dim2]>=100/ni
  if (points=='best') condi <- resmca$ind$contrib[,dim1]>=100/ni | resmca$ind$contrib[,dim2]>=100/ni
  icoord <- as.data.frame(resmca$ind$coord[condi,axes])
  names(icoord) <- c('axeX','axeY')
  if(length(col)==ni) col <- col[condi]
  
  if(type=='i' & is.character(col)) p <- ggplot2::ggplot(icoord, ggplot2::aes(x = .data$axeX, y = .data$axeY)) +  
                                         ggplot2::geom_point(size = size, alpha = alpha, colour = col) + 
                                         ggplot2::guides(colour = FALSE)
  
  if(type=='i' & is.factor(col)) {
    p <- ggplot2::ggplot(icoord, ggplot2::aes(x = .data$axeX, y = .data$axeY)) +
         ggplot2::geom_point(ggplot2::aes(colour = col), size = size, alpha = alpha) +
         ggplot2::guides(colour = ggplot2::guide_legend(title=""))
    if(palette %in% rownames(RColorBrewer::brewer.pal.info)) { p <- p + ggplot2::scale_color_brewer(palette = palette)
    } else if(palette=='bw') { p <- p + ggplot2::scale_color_grey()
    } else if(length(palette)>1) { p <- p + ggplot2::scale_colour_manual(values = palette) }
  }
  
  if(type=='inames' & is.character(col)) {
    p <- ggplot2::ggplot(icoord, ggplot2::aes(x = .data$axeX, y = .data$axeY)) +
         ggplot2::guides(colour = FALSE)
    if(repel==TRUE) { p <- p + ggrepel::geom_text_repel(label = rownames(icoord), size = size, alpha = alpha, colour = col)
      } else { p <- p + ggplot2::geom_text(label = rownames(icoord), size = size, alpha = alpha, colour = col) }
    }
  
  if(type=='inames' & is.factor(col)) { 
    p <- ggplot2::ggplot(icoord, ggplot2::aes(x = .data$axeX, y = .data$axeY)) +  
         ggplot2::guides(colour = ggplot2::guide_legend(title=""))
    if(palette %in% rownames(RColorBrewer::brewer.pal.info)) { p <- p + ggplot2::scale_color_brewer(palette = palette)
    } else if(palette=='bw') { p <- p + ggplot2::scale_color_grey() 
    } else if(length(palette)>1) { p <- p + ggplot2::scale_colour_manual(values = palette)}
    if(repel==TRUE) { p <- p + ggrepel::geom_text_repel(ggplot2::aes(colour = col), label = rownames(icoord), size = size, alpha = alpha)
      } else { p <- p + ggplot2::geom_text(ggplot2::aes(colour = col), label = rownames(icoord), size = size, alpha = alpha) }
    }
  
  if(!is.null(density)) {
    if(density=="contour") p <- p + ggplot2::stat_density_2d(colour=col.contour, size=0.2)
    if(density=="hex") p <- p + ggplot2::geom_hex(bins=hex.bins) +
                                ggplot2::scale_fill_continuous(type="viridis", option=hex.pal)
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
  
  p + ggplot2::geom_hline(yintercept = 0, colour = "darkgrey", size=.1) +
      ggplot2::geom_vline(xintercept = 0, colour = "darkgrey", size=.1) +
      ggplot2::xlab(paste0("dim ", dim1, " (", round(rate1,1), " %)")) +
      ggplot2::ylab(paste0("dim ", dim2, " (", round(rate2,1), " %)")) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     legend.position="none")
}