# resmca = speMCA(Taste[,1:11], excl=c(3,6,9,12,15,18,21,24,27,30,33))
# axes = c(1,2)
# type = "total1"
# K = 5
# legend = "right"
# palette = "Set3"
# supvars = Taste[,c("Gender", "Age", "Educ")]

# 
# pl <- ggbootvalid_variables(resmca, type = "partial", K = 5, ellipse = "t")
# pl
# pl + paletteer::scale_color_paletteer_d("rcartocolor::Prism")

# pl <- ggcloud_variables(resmca)
# pl
# pl + paletteer::scale_color_paletteer_d("rcartocolor::Prism")


# ggbootvalid_variables(resmca, type = "partial", K = 5, ellipse = "norm", level = 0.5)


ggbootvalid_variables <- function(resmca, axes = c(1,2), type = "partial", K = 30,
                                  ellipse = "norm", level = 0.95, palette = NULL, legend = "right") {

  classe <- attr(resmca,'class')[1]
  if(classe %in% c("MCA","speMCA","csMCA")) {
    rate1 <- modif.rate(resmca)$modif$mrate[axes[1]]
    rate2 <- modif.rate(resmca)$modif$mrate[axes[2]]
  }
  if(classe %in% c("stMCA","multiMCA","PCA")) {
    rate1 <- modif.rate(resmca)$raw$rate[axes[1]]
    rate2 <- modif.rate(resmca)$raw$rate[axes[2]]
  }

  noms <- getvarnames(resmca)
    
  boot <- bootvalid_variables(resmca, axes = axes, type = type, K = K)
  names(boot)[3:4] <- c("dimX", "dimY")
  boot <- merge(boot, noms, by = "varcat")
  
  coord <- data.frame(resmca$var$coord[, axes])
  names(coord) <- c("dimX", "dimY")
  coord$varcat <- rownames(coord)
  coord <- merge(coord, noms, by = "varcat")
  
  p <- ggplot2::ggplot() +
    ggrepel::geom_text_repel(data = coord, ggplot2::aes(x = .data$dimX, y = .data$dimY, label = .data$varcat, col = .data$var), key_glyph='blank', parse = TRUE, max.overlaps = Inf) + 
    ggplot2::stat_ellipse(data = boot,  ggplot2::aes(x = .data$dimX, y = .data$dimY, group = .data$varcat, col = .data$var), type = ellipse, level = level, lty = 2) +
    ggplot2::geom_point(data = boot,  ggplot2::aes(x = .data$dimX, y = .data$dimY, col = .data$var), alpha = 0.2) +
    ggplot2::geom_hline(yintercept = 0, colour = "darkgrey", linewidth = .1) + 
    ggplot2::geom_vline(xintercept = 0, colour = "darkgrey", linewidth = .1) + 
    ggplot2::xlab(paste0("dim ", axes[1], " (", round(rate1,1), " %)")) +
    ggplot2::ylab(paste0("dim ", axes[2], " (", round(rate2,1), " %)")) +
    ggplot2::theme_bw() + 
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::guides(color = ggplot2::guide_legend(title = ""))

  if(!is.null(palette)) {
    if(length(palette)>1) { 
      p <- p + 
            ggplot2::scale_colour_manual(values = palette) +
            ggplot2::theme(legend.position = legend)
    } else if(length(palette)==1) {
      if(palette %in% rownames(RColorBrewer::brewer.pal.info)) {
        p <- p + 
              ggplot2::scale_color_brewer(palette = palette) +
              ggplot2::theme(legend.position = legend)
      # } else if(palette=='bw') { p <- p + ggplot2::scale_color_grey() 
      } else if(is.character(palette)) { 
        p <- p + 
              ggplot2::scale_colour_manual(values = rep(palette, length(unique(coord$var)))) +
              ggplot2::theme(legend.position = "none")
        }
    }
  }
    
  p
}
