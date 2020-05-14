ggcloud_variables <- function(resmca, axes=c(1,2), points='all', shapes=TRUE, prop=NULL, textsize=3, shapesize=3, palette=NULL, alpha=1, segment.alpha=0.5, vlab=TRUE, sep='.', legend='right') {

  dim1 <- axes[1]
  dim2 <- axes[2]
  vcoord <- as.data.frame(resmca$var$coord[,axes])
  names(vcoord) <- c('axeX','axeY')
  nk <- nrow(vcoord)
  
  if(is.null(prop)) { vcoord$prop <- rep(3,nk)
  } else if(prop=='n') { vcoord$prop <- resmca$var$weight
  } else if(prop=='ctr1') { vcoord$prop <- resmca$var$contrib[,dim1] 
  } else if(prop=='ctr2') { vcoord$prop <- resmca$var$contrib[,dim2] 
  } else if(prop=='ctr.cloud') { vcoord$prop <- unlist(resmca$var$ctr.cloud)
  } else if(prop=='cos1') { vcoord$prop <- resmca$var$cos2[,dim1] 
  } else if(prop=='cos2') { vcoord$prop <- resmca$var$cos2[,dim2] 
  } else if(prop=='cos12') vcoord$prop <- rowSums(resmca$var$contrib[,axes])
  
  if(points=='all') { condi <- rep(TRUE,nk)
  } else if (points=='besth') { condi <- resmca$var$contrib[,dim1]>=100/nk
  } else if (points=='bestv') { condi <- resmca$var$contrib[,dim2]>=100/nk
  } else if (points=='best') { condi <- resmca$var$contrib[,dim1]>=100/nk | resmca$var$contrib[,dim2]>=100/nk }
  
  nlev <- sapply(resmca$call$X, nlevels)
  vnames <- names(resmca$call$X)
  variables <- character()
  for(i in 1:length(vnames)) variables <- c(variables, rep(vnames[i], nlev[i]))
  categories <- unlist(lapply(resmca$call$X, levels))
  names(categories) <- NULL
  varcat <- apply(cbind(variables, categories), 1, paste, collapse=sep)
  
  if(class(resmca)[1] %in% c('csMCA','speMCA')) {
    categories <- categories[-resmca$call$excl]
    variables <- variables[-resmca$call$excl]
    varcat <- varcat[-resmca$call$excl]
  }
  
  vcoord$variables <- variables
  vcoord$categories <- categories
  vcoord$varcat <- varcat
  
  if(vlab) vcoord$labs <- varcat else vcoord$labs <- categories

  p <- ggplot2::ggplot(vcoord, ggplot2::aes(x = .data$axeX, y = .data$axeY))
  
  # if(shapes==TRUE & is.null(palette) & is.null(prop)) p <- p + ggplot2::geom_point(data=subset(vcoord, condi), ggplot2::aes(shape = .data$variables), size = shapesize, colour = col, alpha = alpha) + 
  #                                                              ggrepel::geom_text_repel(data=subset(vcoord, condi), ggplot2::aes(label = .data$labs), size = textsize, segment.alpha = segment.alpha, colour = col, alpha = alpha) + 
  #                                                              ggplot2::scale_shape_manual(name="", values = 0:20)
  # if(shapes==TRUE & is.null(palette) & !is.null(prop)) p <- p + ggplot2::geom_point(data=subset(vcoord, condi), ggplot2::aes(shape = .data$variables, size = .data$prop), colour = col, alpha = alpha) + 
  #                                                               ggrepel::geom_text_repel(data=subset(vcoord, condi), ggplot2::aes(label = .data$labs), size = textsize, segment.alpha = segment.alpha, colour = col, alpha = alpha) + 
  #                                                               ggplot2::scale_shape_manual(name="", values = 0:20)
  
  if(shapes==TRUE & is.null(prop)) p <- p + ggplot2::geom_point(data=subset(vcoord, condi), ggplot2::aes(shape = .data$variables, color = .data$variables), size = shapesize, alpha = alpha) + 
                                                                ggrepel::geom_text_repel(key_glyph='blank', data=subset(vcoord, condi), ggplot2::aes(label = .data$labs, color = .data$variables), size = textsize, segment.alpha = segment.alpha, alpha = alpha) + 
                                                                ggplot2::scale_shape_manual(name="", values = 0:20) +
  if(shapes==TRUE & !is.null(prop)) p <- p + ggplot2::geom_point(data=subset(vcoord, condi), ggplot2::aes(shape = .data$variables, size = .data$prop, color = .data$variables), alpha = alpha) + 
                                                                 ggrepel::geom_text_repel(key_glyph='blank', data=subset(vcoord, condi), ggplot2::aes(label = .data$labs, color = .data$variables), size = textsize, segment.alpha = segment.alpha, alpha = alpha) + 
                                                                 ggplot2::scale_shape_manual(name="", values = 0:20) +
                                                                 
#  if(shapes==FALSE & is.null(palette) & is.null(prop)) p <- p + ggrepel::geom_text_repel(data=subset(vcoord, condi), ggplot2::aes(label = .data$labs), size = textsize, segment.alpha = segment.alpha, colour = col, alpha = alpha)
#  if(shapes==FALSE & is.null(palette) & !is.null(prop)) p <- p + ggrepel::geom_text_repel(data=subset(vcoord, condi), ggplot2::aes(label = .data$labs, size = .data$prop), segment.alpha = segment.alpha, colour = col, alpha = alpha)
  
  if(shapes==FALSE & is.null(prop)) p <- p + ggrepel::geom_text_repel(key_glyph='point', data=subset(vcoord, condi), ggplot2::aes(label = .data$labs, color = .data$variables), size = textsize, segment.alpha = segment.alpha, alpha = alpha)
  if(shapes==FALSE & !is.null(prop)) p <- p + ggrepel::geom_text_repel(key_glyph='point', data=subset(vcoord, condi), ggplot2::aes(label = .data$labs, size = .data$prop, color = .data$variables), segment.alpha = segment.alpha, alpha = alpha)

  if(!is.null(palette)) {
    if(length(palette)>1) { p <- p + ggplot2::scale_colour_manual(values = palette)
    } else if(length(palette)==1) {
      if(palette %in% rownames(RColorBrewer::brewer.pal.info)) { p <- p + ggplot2::scale_color_brewer(palette = palette)
      } else if(palette=='bw') { p <- p + ggplot2::scale_color_grey() 
      } else if(is.character(palette)) { p <- p + ggplot2::scale_colour_manual(values = rep(palette,length(vnames))) }
    }
  }
      
  p + ggplot2::geom_hline(yintercept = 0, colour = "darkgrey", linetype="longdash") + 
      ggplot2::geom_vline(xintercept = 0, colour = "darkgrey", linetype="longdash") + 
    
      ggplot2::xlab(paste0("Axe ", dim1, " (", round(resmca$eig$mrate[dim1],1), " %)")) +
      ggplot2::ylab(paste0("Axe ", dim2, " (", round(resmca$eig$mrate[dim2],1), " %)")) +
    
      ggplot2::theme_minimal() + 
      
      ggplot2::theme(panel.border = ggplot2::element_rect(colour="darkgrey", fill=NA)) +
      
      ggplot2::guides(shape = ggplot2::guide_legend(title=""), color = ggplot2::guide_legend(title=""), size = FALSE) + 
      ggplot2::theme(legend.position = legend)
}