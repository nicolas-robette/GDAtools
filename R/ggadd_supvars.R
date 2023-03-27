ggadd_supvars <- function(p, resmca, vars, axes = c(1,2), col = NULL, 
                          shapes = FALSE, prop = NULL, textsize = 3, shapesize = 6, vname = TRUE) {

  if(any(sapply(vars, FUN = function(x) !is.factor(x)))) stop("variables in data should all be factors")

  dim1 <- axes[1]
  dim2 <- axes[2]
  vs <- supvars(resmca, vars)
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

  if(!shapes) {
    if(is.null(col)) {
      if(is.null(prop)) {
        pfin <- p +
          ggrepel::geom_text_repel(data = coord, ggplot2::aes(x = .data$axeX, y = .data$axeY, label = .data$labs, color = .data$vnames), size = textsize) +
          ggplot2::guides(color = "none")        
      } else {
        pfin <- p +
          ggrepel::geom_text_repel(data = coord, ggplot2::aes(x = .data$axeX, y = .data$axeY, label = .data$labs, size = .data$prop, color = .data$vnames)) +
          ggplot2::guides(color = "none")
      }
    } else {
      if(is.null(prop)) {
        pfin <- p +
          ggrepel::geom_text_repel(data = coord, ggplot2::aes(x = .data$axeX, y = .data$axeY, label = .data$labs), size = textsize, color = col)      
      } else {
        pfin <- p +
          ggrepel::geom_text_repel(data = coord, ggplot2::aes(x = .data$axeX, y = .data$axeY, label = .data$labs, size = .data$prop), color = col)      
      }
    }
  } else {
    if(is.null(col)) {
      pfin <- p +
        ggplot2::geom_point(data = coord, ggplot2::aes(x = .data$axeX, y = .data$axeY, size = .data$prop, color = .data$vnames, shape = .data$vnames)) +
        ggplot2::scale_size_area(max_size = shapesize) +
        ggrepel::geom_text_repel(data = coord, ggplot2::aes(x = .data$axeX, y = .data$axeY, label = .data$labs, color = .data$vnames), size = textsize) +
        ggplot2::guides(color = "none", shape = "none")
    } else {
      pfin <- p +
        ggplot2::geom_point(data = coord, ggplot2::aes(x = .data$axeX, y = .data$axeY, size = .data$prop, shape = .data$vnames), color = col) +
        ggplot2::scale_size_area(max_size = shapesize) +
        ggrepel::geom_text_repel(data = coord, ggplot2::aes(x = .data$axeX, y = .data$axeY, label = .data$labs), size = textsize, color = col) +
        ggplot2::guides(shape = "none")
    }
  }

  pfin
}
