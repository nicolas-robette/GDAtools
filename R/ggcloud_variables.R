ggcloud_variables <- function(resmca, axes=c(1,2), points='all', min.ctr=NULL, max.pval=0.01, face="pp", shapes=TRUE, prop=NULL, textsize=3, shapesize=3, col=NULL, col.by.group=TRUE, alpha=1, segment.alpha=0.5, vlab=TRUE, sep='.', legend='right',
                              force = 1, max.overlaps = Inf) {

  type <- attr(resmca,'class')[1]
  dim1 <- axes[1]
  dim2 <- axes[2]
  
  if(type %in% c("MCA","speMCA","csMCA")) {
    rate1 <- modif.rate(resmca)$modif$mrate[dim1]
    rate2 <- modif.rate(resmca)$modif$mrate[dim2]
  }
  if(type %in% c("stMCA","multiMCA","PCA")) {
    rate1 <- modif.rate(resmca)$raw$rate[dim1]
    rate2 <- modif.rate(resmca)$raw$rate[dim2]
  }
  
  if(type=="MCA") resmca$call$X <- resmca$call$X[,resmca$call$quali]
  
  if(type=="stMCA") {
    for(i in 1:ncol(resmca$call$X)) levels(resmca$call$X[,i]) <- gsub(names(resmca$call$X)[i],"",gsub("_","",levels(resmca$call$X[,i])))
  }
  
  if(type=="multiMCA") {
    listX <- lapply(resmca$my.mca, function(x) x$call$X)
    nk <- sapply(listX, function(x) ncol(dichotom(x)))
    listexcl <- lapply(resmca$my.mca, function(x) x$call$excl)
    for(i in 2:length(listexcl)) listexcl[[i]] <- listexcl[[i]] + cumsum(nk)[i-1]
    resmca$call$excl <- unlist(listexcl)
    resmca$call$X <- do.call("cbind.data.frame", listX)
    resmca$var$coord <- do.call("rbind.data.frame", lapply(resmca$VAR, function(x) x$coord))
    resmca$var$v.test <- do.call("rbind.data.frame", lapply(resmca$VAR, function(x) x$v.test))
    groups <- numeric()
    for(i in 1:length(resmca$VAR)) groups <- c(groups, rep(i, nrow(resmca$VAR[[i]]$coord)))
    groups <- factor(groups)
  }
  
  vcoord <- as.data.frame(resmca$var$coord[,axes])
  names(vcoord) <- c('axeX','axeY')
  nk <- nrow(vcoord)
  
  if(is.null(prop)) { vcoord$prop <- rep(3,nk)
  } else if(prop=='n') { vcoord$prop <- resmca$var$weight
  } else if(prop=='ctr1') { vcoord$prop <- resmca$var$contrib[,dim1] 
  } else if(prop=='ctr2') { vcoord$prop <- resmca$var$contrib[,dim2] 
  } else if(prop=='ctr12') { vcoord$prop <- planecontrib(resmca, axes)$var$ctr
  } else if(prop=='ctr.cloud') { vcoord$prop <- unlist(resmca$var$ctr.cloud)
  } else if(prop=='cos1') { vcoord$prop <- resmca$var$cos2[,dim1] 
  } else if(prop=='cos2') { vcoord$prop <- resmca$var$cos2[,dim2] 
  } else if(prop=='cos12') { vcoord$prop <- rowSums(resmca$var$cos2[,axes])
  } else if(prop=='vtest1') { vcoord$prop <- abs(resmca$var$v.test[,dim1])
  } else if(prop=='vtest2') vcoord$prop <- abs(resmca$var$v.test[,dim2])

  if(type %in% c("MCA","speMCA","csMCA")) {  
    if(is.null(min.ctr)) min.ctr <- 100/nk
    if(points=='all') { condi <- rep(TRUE,nk)
    } else if (points=='besth') { condi <- resmca$var$contrib[,dim1]>=min.ctr
    } else if (points=='bestv') { condi <- resmca$var$contrib[,dim2]>=min.ctr
    } else if (points=='besthv') { condi <- resmca$var$contrib[,dim1]>=min.ctr | resmca$var$contrib[,dim2]>=min.ctr
    } else if (points=='best') { condi <- planecontrib(resmca, axes)$var$ctr>=min.ctr }
  }
  
  if(type %in% c("stMCA","multiMCA")) {
    if(points=='all') { condi <- rep(TRUE,nk)
    } else if (points=='besth') { condi <- 2*(1 -stats::pnorm(abs(resmca$var$v.test[,dim1])))<=max.pval
    } else if (points=='bestv') { condi <- 2*(1 -stats::pnorm(abs(resmca$var$v.test[,dim2])))<=max.pval
    } else if (points=='best') { condi <- 2*(1 -stats::pnorm(abs(resmca$var$v.test[,dim1])))<=max.pval | 2*(1 -stats::pnorm(abs(resmca$var$v.test[,dim2])))<=max.pval }
  }
  
  nlev <- sapply(resmca$call$X, nlevels)
  vnames <- names(resmca$call$X)
  variables <- character()
  for(i in 1:length(vnames)) variables <- c(variables, rep(vnames[i], nlev[i]))
  categories <- unlist(lapply(resmca$call$X, levels))
  names(categories) <- NULL
  varcat <- apply(cbind(variables, categories), 1, paste, collapse=sep)
  
  if(type %in% c("csMCA","speMCA","stMCA","multiMCA")) {
    categories <- categories[-resmca$call$excl]
    variables <- variables[-resmca$call$excl]
    varcat <- varcat[-resmca$call$excl]
  }
  
  vcoord$variables <- factor(variables, levels=names(resmca$call$X))
  vcoord$categories <- categories
  vcoord$varcat <- varcat
  if(type=="multiMCA" & col.by.group==TRUE) vcoord$groups <- groups else vcoord$groups <- vcoord$variables
  
  if(vlab) vcoord$labs <- varcat else vcoord$labs <- categories

  face <- unlist(strsplit(face, split = ""))
  faceX <- face[1]
  faceY <- face[2]
  vcoord$labs <- paste0("'",vcoord$labs,"'")
  if(type %in% c("MCA","speMCA","csMCA")) {
    vcoord$labs[(resmca$var$contrib[,dim1]>=min.ctr & faceX=="i") | (resmca$var$contrib[,dim2]>=min.ctr & faceY=="i")] <- 
      paste0("italic(",vcoord$labs[(resmca$var$contrib[,dim1]>=min.ctr & faceX=="i") | (resmca$var$contrib[,dim2]>=min.ctr & faceY=="i")],")")
    vcoord$labs[(resmca$var$contrib[,dim1]>=min.ctr & faceX=="b") | (resmca$var$contrib[,dim2]>=min.ctr & faceY=="b")] <- 
      paste0("bold(",vcoord$labs[(resmca$var$contrib[,dim1]>=min.ctr & faceX=="b") | (resmca$var$contrib[,dim2]>=min.ctr & faceY=="b")],")")
    vcoord$labs[(resmca$var$contrib[,dim1]>=min.ctr & faceX=="u") | (resmca$var$contrib[,dim2]>=min.ctr & faceY=="u")] <- 
      paste0("underline(",vcoord$labs[(resmca$var$contrib[,dim1]>=min.ctr & faceX=="u") | (resmca$var$contrib[,dim2]>=min.ctr & faceY=="u")],")")
  }
  
  p <- ggplot2::ggplot(vcoord, ggplot2::aes(x = .data$axeX, y = .data$axeY))
  
  if(shapes==TRUE & is.null(prop) & is.null(col)) p <- p + ggplot2::geom_point(data=subset(vcoord, condi), ggplot2::aes(shape = .data$variables, color = .data$groups), size = shapesize, alpha = alpha) + 
                                                           ggrepel::geom_text_repel(key_glyph='blank',
                                                                                    data=subset(vcoord, condi),
                                                                                    ggplot2::aes(label = .data$labs, color = .data$groups),
                                                                                    size = textsize, segment.alpha = segment.alpha, alpha = alpha, parse = TRUE, force = force, max.overlaps = max.overlaps) + 
                                                           ggplot2::scale_shape_manual(name="", values = rep(0:20,10))

  if(shapes==TRUE & is.null(prop) & !is.null(col)) p <- p + ggplot2::geom_point(data=subset(vcoord, condi), ggplot2::aes(shape = .data$variables), color = col, size = shapesize, alpha = alpha) + 
                                                            ggrepel::geom_text_repel(key_glyph='blank',
                                                                                     data=subset(vcoord, condi),
                                                                                     ggplot2::aes(label = .data$labs),
                                                                                     color = col, size = textsize, segment.alpha = segment.alpha, alpha = alpha, parse = TRUE, force = force, max.overlaps = max.overlaps) + 
                                                            ggplot2::scale_shape_manual(name="", values = rep(0:20,10))  
  
  if(shapes==TRUE & !is.null(prop) & is.null(col)) p <- p + ggplot2::geom_point(data=subset(vcoord, condi), ggplot2::aes(shape = .data$variables, size = .data$prop, color = .data$groups), alpha = alpha) + 
                                                            ggrepel::geom_text_repel(key_glyph='blank',
                                                                                     data=subset(vcoord, condi),
                                                                                     ggplot2::aes(label = .data$labs, color = .data$groups),
                                                                                     size = textsize, segment.alpha = segment.alpha, alpha = alpha, parse = TRUE, force = force, max.overlaps = max.overlaps) + 
                                                            ggplot2::scale_shape_manual(name="", values = rep(0:20,10))

  if(shapes==TRUE & !is.null(prop) & !is.null(col)) p <- p + ggplot2::geom_point(data=subset(vcoord, condi), ggplot2::aes(shape = .data$variables, size = .data$prop), color = col, alpha = alpha) + 
                                                             ggrepel::geom_text_repel(key_glyph='blank',
                                                                                      data=subset(vcoord, condi),
                                                                                      ggplot2::aes(label = .data$labs),
                                                                                      color = col, size = textsize, segment.alpha = segment.alpha, alpha = alpha, parse = TRUE, force = force, max.overlaps = max.overlaps) + 
                                                             ggplot2::scale_shape_manual(name="", values = rep(0:20,10))  
                                                                   
  if(shapes==FALSE & is.null(prop) & is.null(col)) p <- p + ggrepel::geom_text_repel(key_glyph='point',
                                                                                     data=subset(vcoord, condi),
                                                                                     ggplot2::aes(label = .data$labs, color = .data$groups),
                                                                                     size = textsize, segment.alpha = segment.alpha, alpha = alpha, parse = TRUE, force = force, max.overlaps = max.overlaps)

  if(shapes==FALSE & is.null(prop) & !is.null(col)) p <- p + ggrepel::geom_text_repel(key_glyph='point',
                                                                                      data=subset(vcoord, condi),
                                                                                      ggplot2::aes(label = .data$labs),
                                                                                      color = col, size = textsize, segment.alpha = segment.alpha, alpha = alpha, parse = TRUE, force = force, max.overlaps = max.overlaps)
  
  if(shapes==FALSE & !is.null(prop) & is.null(col)) p <- p + ggrepel::geom_text_repel(key_glyph='point',
                                                                                      data=subset(vcoord, condi),
                                                                                      ggplot2::aes(label = .data$labs, size = .data$prop, color = .data$groups),
                                                                                      segment.alpha = segment.alpha, alpha = alpha, parse = TRUE, force = force, max.overlaps = max.overlaps)
  
  if(shapes==FALSE & !is.null(prop) & !is.null(col)) p <- p + ggrepel::geom_text_repel(key_glyph='point',
                                                                                       data=subset(vcoord, condi),
                                                                                       ggplot2::aes(label = .data$labs, size = .data$prop),
                                                                                       color = col, segment.alpha = segment.alpha, alpha = alpha, parse = TRUE, force = force, max.overlaps = max.overlaps)

  p <- p + 
      ggplot2::geom_hline(yintercept = 0, colour = "darkgrey", linewidth = .1) + 
      ggplot2::geom_vline(xintercept = 0, colour = "darkgrey", linewidth = .1) + 
    
      ggplot2::xlab(paste0("dim ", dim1, " (", round(rate1,1), " %)")) +
      ggplot2::ylab(paste0("dim ", dim2, " (", round(rate2,1), " %)")) +
    
      ggplot2::theme_bw() + 
      
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank()) +
      
      ggplot2::guides(shape = ggplot2::guide_legend(title=""), color = ggplot2::guide_legend(title=""), size = "none") + 
      ggplot2::theme(legend.position = legend)
  
  return(p)
}
