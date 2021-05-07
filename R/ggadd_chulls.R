ggadd_chulls <- function(p, resmca, var, sel=1:nlevels(var), axes=c(1,2), col=NULL, alpha=0.2, label=TRUE, label.size=5, legend="right") {

  subvar <- var
  
  type <- attr(resmca,'class')[1]
  if(type=="stMCA") type <- resmca$call$input.mca
  if(type=="csMCA") subvar <- var[resmca$call$subcloud]
  if(type=="multiMCA") {
    if(class(resmca$my.mca[[1]])[1]=="csMCA") subvar <- var[resmca$my.mca[[1]]$call$subcloud]
  }
    
  ecoord <- as.data.frame(resmca$ind$coord[,axes])
  names(ecoord) <- c('axeX','axeY')
  ecoord$var <- subvar
  ecoord <- ecoord[subvar %in% levels(subvar)[sel],]
  ecoord$var <- factor(ecoord$var)
  
  vs <- varsup(resmca,var)
  ccoord <- as.data.frame(vs$coord[,axes])
  names(ccoord) <- c('axeX','axeY')
  ccoord$categories <- names(vs$weight)
  ccoord <- ccoord[sel,]
  ccoord$axeX <- ccoord$axeX*resmca$svd$vs[axes[1]]
  ccoord$axeY <- ccoord$axeY*resmca$svd$vs[axes[2]]
  
  pfin <- p + stat_chull(data=ecoord, ggplot2::aes(x=.data$axeX, y=.data$axeY, colour=.data$var, fill=.data$var), alpha=alpha)
  
  if(label) pfin <- pfin + ggplot2::geom_text(key_glyph='blank', data=ccoord, ggplot2::aes(x=.data$axeX, y=.data$axeY, label=.data$categories, colour=.data$categories), size=label.size)

  if(!is.null(col)) {
    if(length(col)>1) { pfin <- pfin + ggplot2::scale_colour_manual(values = col) +
                                       ggplot2::scale_fill_manual(values = col)
    } else if(length(col)==1) {
      if(col %in% rownames(RColorBrewer::brewer.pal.info)) { pfin <- pfin + ggplot2::scale_color_brewer(palette = col) +
                                                                            ggplot2::scale_fill_brewer(palette = col)
      } else if(col=='bw') { pfin <- pfin + ggplot2::scale_color_grey() +
                                            ggplot2::scale_fill_grey()
      } else if(is.character(col)) { pfin <- pfin + ggplot2::scale_colour_manual(values = rep(col,nrow(ecoord))) +
                                                    ggplot2::scale_fill_manual(values = rep(col,nrow(ecoord)))
        }
    }
  }
    
  pfin <- pfin + ggplot2::guides(color = ggplot2::guide_legend(title=""),
                                 fill =  ggplot2::guide_legend(title="")) +
                 ggplot2::theme(legend.position = legend)
  
  pfin
}


