# library(GDAtools)
# data(Music)
# getindexcat(Music)
# resmca <- speMCA(Music[,1:5],excl=c(3,6,9,12,15))
# vars <- Music[,6:9]
# 
# library(ggnewscale)
# prop = "cos12"
# 
# p <- ggcloud_variables(resmca, palette='lightgrey', shapes=FALSE)
# ggadd_supvars(p, resmca, vars)
# ggadd_supvars(p, resmca, vars, shape = FALSE, prop = NULL, textsize = 15)
# ggadd_supvars(p, resmca, vars, prop = "cos12", shape = TRUE, shapesize = 15, textsize = 1)
# ggadd_supvars(p, resmca, vars, prop = "cos12", shape = TRUE, vname = FALSE)

ggadd_supvars <- function(p, resmca, vars, axes=c(1,2), palette = "khroma::bright", 
                          shape=FALSE, prop=NULL, textsize=3, shapesize=6, vname=TRUE) {

  if(any(sapply(vars, FUN = function(x) !is.factor(x)))) stop("variables in data should all be factors")

  dim1 <- axes[1]
  dim2 <- axes[2]
  vs <- varsups(resmca, vars)
  coord <- as.data.frame(vs$coord[,axes])
  names(coord) <- c('axeX','axeY')
  coord$n <- vs$weight
  coord$categories <- unlist(sapply(vars, levels))
  nlev <- sapply(vars, nlevels)
  vnames <- character()
  for(i in 1:length(nlev)) vnames <- c(vnames, rep(names(vars[i]), nlev[i]))
  coord$vnames <- vnames
  coord$labs <- coord$categories
  if(vname) coord$labs <- paste(coord$vnames, coord$labs, sep='.')
  coord$vnames <- factor(coord$vnames)

  if(is.null(prop)) { coord$prop <- rep(1, nrow(coord))
  } else if(prop=='n') { coord$prop <- vs$weight
  } else if(prop=='vtest1') { coord$prop <- abs(vs$typic[,dim1])
  } else if(prop=='vtest2') { coord$prop <- abs(vs$typic[,dim2])
  } else if(prop=='cos1') { coord$prop <- vs$cos2[,dim1]
  } else if(prop=='cos2') { coord$prop <- vs$cos2[,dim2]
  } else if(prop=='cos12') coord$prop <- rowSums(vs$cos2[,axes])

  # levs <- names(vs$weight) %in% levels(var)[sel]
  # coord <- coord[levs,]

  old.warn <- options()$warn
  options(warn = -1)

  # p + ggplot2::guides(color = "none") +
  #     ggnewscale::new_scale_color() +
  #     ggrepel::geom_text_repel(data=coord, ggplot2::aes(x=.data$axeX, y=.data$axeY, label=.data$labs, size=.data$prop, color=.data$vnames)) +
  #     ggplot2::guides(color = "none")

  if(!shape) {
    pfin <- p +
      ggplot2::guides(color = "none") +
      ggnewscale::new_scale_color() +
      ggrepel::geom_text_repel(data=coord, ggplot2::aes(x=.data$axeX, y=.data$axeY, label=.data$labs, size=.data$prop, color=.data$vnames), size=textsize) +
      paletteer::scale_color_paletteer_d(palette = palette) +
      ggplot2::guides(color = "none")
  } else {
    pfin <- p +
      ggplot2::guides(color = "none") +
      ggnewscale::new_scale_color() +
      ggplot2::geom_point(data=coord, ggplot2::aes(x=.data$axeX, y=.data$axeY, size=.data$prop, color=.data$vnames, shape=.data$vnames)) +
      ggplot2::scale_size_area(max_size=shapesize) +
      paletteer::scale_color_paletteer_d(palette = palette) +
      ggrepel::geom_text_repel(data=coord, ggplot2::aes(x=.data$axeX, y=.data$axeY, label=.data$labs, color=.data$vnames), size=textsize) +
      ggplot2::guides(color = "none", shape = "none")
  }

  options(warn = old.warn)
  pfin
}
