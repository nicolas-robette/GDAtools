ggadd_interaction <- function(p, 
                              resmca,
                              v1,
                              v2,
                              sel1 = 1:nlevels(v1),
                              sel2 = 1:nlevels(v2),
                              axes = c(1,2),
                              cloud = "v",
                              textsize = 5,
                              lines = TRUE,
                              dashes = TRUE,
                              legend = "none",
                              force = 1, 
                              max.overlaps = Inf) {

  type <- attr(resmca,'class')[1]
  
  if(type=="bcMCA") {
    wt <- resmca$mycall$row.w
  } else if(type=="csMCA") {
    wt <- resmca$call$row.w[resmca$call$subcloud]
  } else {
    wt <- resmca$call$row.w
  }

  v12 <- interaction(v1,v2)

  if(cloud == "v") {
    vs <- supvar(resmca, v12)
    coord <- as.data.frame(vs$coord[,axes])
    coord$cat1 <- sub("\\..*", "", names(vs$weight))
    coord$cat2 <- sub(".*\\.", "", names(vs$weight))
    coord$cat12 <- names(vs$weight)    
  } else if(cloud == "i") {
    coord <- agg.wtd.mean(resmca$ind$coord[,axes], v12, wt)
    coord$cat1 <- sub("\\..*", "", rownames(coord))
    coord$cat2 <- sub(".*\\.", "", rownames(coord))
    coord$cat12 <- rownames(coord)
  }

  names(coord)[1:2] <- c('axeX','axeY')
  coord <- coord[coord$cat1 %in% levels(v1)[sel1] & coord$cat2 %in% levels(v2)[sel2],]
  coord$cat1 <- factor(coord$cat1)
  coord$cat2 <- factor(coord$cat2)
  coord$cat12 <- factor(coord$cat12)
  
  p <- p + ggplot2::geom_point(data=coord, ggplot2::aes(color=.data$cat1)) +
           ggrepel::geom_text_repel(key_glyph='blank', 
                                    data=coord, 
                                    ggplot2::aes(color=.data$cat1, label=.data$cat12),
                                    size = textsize,
                                    force = force, 
                                    max.overlaps = max.overlaps)
  
  if(lines) p <- p +
    ggplot2::geom_path(data=coord, 
                       ggplot2::aes(color=.data$cat1))
  
  if(dashes) p <- p +
    ggplot2::geom_path(data = coord, 
                       ggplot2::aes(x = .data$axeX,
                                    y = .data$axeY,
                                    group = .data$cat2),
                       color = "darkgray", 
                       linetype = "dashed")
  
  # if(!is.null(col)) {
  #   if(length(col)>1) { p <- p + ggplot2::scale_colour_manual(values = col)
  #   } else if(length(col)==1) {
  #     if(col %in% rownames(brewer.pal.info)) { p <- p + ggplot2::scale_color_brewer(palette = col)
  #     } else if(col=='bw') { p <- p + ggplot2::scale_color_grey() 
  #     } else if(is.character(col)) { p <- p + ggplot2::scale_colour_manual(values = rep(col,nrow(coord))) }
  #   }
  # }
  
  p <- p + ggplot2::guides(shape = "none", color = ggplot2::guide_legend(title="")) + 
           ggplot2::theme(legend.position = legend)
  
  p
}
