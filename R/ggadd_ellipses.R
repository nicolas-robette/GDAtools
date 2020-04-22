ggadd_ellipses <- function(p, resmca, var, sel=1:nlevels(var), axes=c(1,2), label=TRUE, col=NULL, legend='right', level=0.86, alpha=0.5) {

  ecoord <- as.data.frame(resmca$ind$coord[,axes])
  names(ecoord) <- c('axeX','axeY')
  ecoord$var <- var
  ecoord <- ecoord[var %in% levels(var)[sel],]
  ecoord$var <- factor(ecoord$var)

  vs <- varsup(resmca,var)
  ccoord <- as.data.frame(vs$coord[,axes])
  names(ccoord) <- c('axeX','axeY')
  ccoord$categories <- names(vs$weight)
  ccoord <- ccoord[sel,]
  ccoord$axeX <- ccoord$axeX*resmca$svd$vs[axes[1]]
  ccoord$axeY <- ccoord$axeY*resmca$svd$vs[axes[2]]

  pfin <- p + ggplot2::stat_ellipse(data=ecoord, ggplot2::aes(x=.data$axeX, y=.data$axeY, colour=.data$var), level = level, type='norm') +
              ggplot2::geom_point(data=ecoord, ggplot2::aes(x=.data$axeX, y=.data$axeY, colour=.data$var), alpha = alpha)
  
  if(!is.null(col)) {
    if(length(col)>1) { pfin <- pfin + ggplot2::scale_colour_manual(values = col)
    } else if(length(col)==1) {
      if(col %in% rownames(RColorBrewer::brewer.pal.info)) { pfin <- pfin + ggplot2::scale_color_brewer(palette = col)
      } else if(col=='bw') { pfin <- pfin + ggplot2::scale_color_grey() 
      } else if(is.character(col)) { pfin <- pfin + ggplot2::scale_colour_manual(values = rep(col,nrow(ecoord))) }
    }
  }
  
  if(label) { pfin <- pfin + ggplot2::geom_text(key_glyph='blank', data=ccoord, ggplot2::aes(x=.data$axeX, y=.data$axeY, label=.data$categories, colour=.data$categories)) 
  } else { pfin <- pfin + ggplot2::geom_point(data=ccoord, ggplot2::aes(x=.data$axeX, y=.data$axeY, colour=.data$categories), shape=8, size=3) }

  # if(legend) pfin <- pfin + guides(color=guide_legend(title="")) + 
  #                           theme(legend.position='right')

  pfin <- pfin + ggplot2::guides(color = ggplot2::guide_legend(title="")) +
                 ggplot2::theme(legend.position = legend)

  pfin

}