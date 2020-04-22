ggadd_supvar <- function(p, resmca, var, sel=1:nlevels(var), axes=c(1,2), col='black', shape=1, prop=NULL, textsize=3, segment=TRUE, vname=NULL) {

  dim1 <- axes[1]
  dim2 <- axes[2]
  vs <- varsup(resmca,var)
  coord <- as.data.frame(vs$coord[,axes])
  names(coord) <- c('axeX','axeY')
  coord$n <- vs$weight
  coord$categories <- names(vs$weight)
  coord$labs <- coord$categories
  if(!(is.null(vname))) coord$labs <- paste(vname, coord$labs, sep='.')
  
  if(is.null(prop)) { coord$prop <- rep(1,nrow(coord))
  } else if(prop=='n') { coord$prop <- vs$weight
  } else if(prop=='vtest1') { coord$prop <- abs(vs$v.test[,dim1])
  } else if(prop=='vtest2') { coord$prop <- abs(vs$v.test[,dim2])
  } else if(prop=='cos1') { coord$prop <- vs$cos2[,dim1] 
  } else if(prop=='cos2') { coord$prop <- vs$cos2[,dim2] 
  } else if(prop=='cos12') coord$prop <- rowSums(vs$contrib[,axes])
  
  levs <- names(vs$weight) %in% levels(var)[sel]
  coord <- coord[levs,]
  
  if(is.null(shape)) { pfin <- p + ggrepel::geom_text_repel(data=coord, ggplot2::aes(x=.data$axeX, y=.data$axeY, label=.data$labs, size=.data$prop), col=col)
  } else { pfin <- p + ggplot2::geom_point(data=coord, ggplot2::aes(x=.data$axeX, y=.data$axeY, size=.data$prop), shape=shape, col=col) +
                       ggrepel::geom_text_repel(data=coord, ggplot2::aes(x=.data$axeX, y=.data$axeY, label=.data$labs), size=textsize, col=col) }
  
  if(segment) pfin <- pfin + ggplot2::geom_line(data=coord, ggplot2::aes(x=.data$axeX, y=.data$axeY), col=col, alpha=0.5)
    
  pfin

}