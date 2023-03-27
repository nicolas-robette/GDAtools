ggadd_interaction <- function(p, resmca, v1, v2, sel1=1:nlevels(v1), sel2=1:nlevels(v2), axes=c(1,2), textsize=5, legend='right') {

  v12 <- interaction(v1,v2)
  vs <- supvar(resmca, v12)
  coord <- as.data.frame(vs$coord[,axes])
  names(coord) <- c('axeX','axeY')
  coord$cat1 <- rep(levels(v1),nlevels(v2))
  coord$cat2 <- unlist(lapply(levels(v2), function(x) rep(x, nlevels(v1))))
  coord$cat12 <- levels(v12)

  coord <- coord[coord$cat1 %in% levels(v1)[sel1] & coord$cat2 %in% levels(v2)[sel2],]
  coord$cat1 <- factor(coord$cat1)
  coord$cat2 <- factor(coord$cat2)
  coord$cat12 <- factor(coord$cat12)
  
  p <- p + ggplot2::geom_point(data=coord, ggplot2::aes(color=.data$cat1)) +
           ggrepel::geom_text_repel(key_glyph='blank', data=coord, ggplot2::aes(color=.data$cat1, label=.data$cat12), size = textsize) +
           ggplot2::geom_path(data=coord, ggplot2::aes(color=.data$cat1)) +
           ggplot2::geom_path(data=coord, ggplot2::aes(group=.data$cat2), color='darkgray', linetype='dashed')

  # if(!is.null(col)) {
  #   if(length(col)>1) { p <- p + ggplot2::scale_colour_manual(values = col)
  #   } else if(length(col)==1) {
  #     if(col %in% rownames(brewer.pal.info)) { p <- p + ggplot2::scale_color_brewer(palette = col)
  #     } else if(col=='bw') { p <- p + ggplot2::scale_color_grey() 
  #     } else if(is.character(col)) { p <- p + ggplot2::scale_colour_manual(values = rep(col,nrow(coord))) }
  #   }
  # }
  
  p <- p + ggplot2::guides(color = ggplot2::guide_legend(title="")) + 
           ggplot2::theme(legend.position = legend)
  
  p
}
