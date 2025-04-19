ggaxis_variables <- function(resmca, var = NULL, axis = 1,
                             min.ctr = NULL,
                             prop = NULL, underline = FALSE, 
                             col = NULL, vlab = TRUE,
                             force = 1, 
                             max.overlaps = Inf) {

  type <- attr(resmca,'class')[1]
  
  if(is.factor(var)) {
    vs <- supvar(resmca,var)
    df <- data.frame(names = names(vs$weight),
                     coord = vs$coord[,paste0("dim.",axis)],
                     freq = vs$weight,
                     cos2 = vs$cos2[,paste0("dim.",axis)],
                     pval = vs$pval[,paste0("dim.",axis)],
                     cor = vs$cor[,paste0("dim.",axis)])
  }
  
  if(is.character(var) & length(var)==1) {
    vs <- resmca$var
    long_names <- grep(var, rownames(vs$coord), value = TRUE)
    short_names <- gsub(paste0(var,"."), "", long_names)
    df <- data.frame(names = short_names,
                     coord = vs$coord[long_names,paste0("dim.",axis)],
                     freq = vs$weight[long_names],
                     cos2 = vs$cos2[long_names,paste0("dim.",axis)],
                     ctr = vs$contrib[long_names,paste0("dim.",axis)])
    if(underline) {
      seuil <- 100/nrow(resmca$var$contrib)
      df$names[df$ctr>=seuil] <- paste0("underline(",df$names[df$ctr>=seuil],")")
    }
  }
  
  if(is.null(var) | (is.character(var) & length(var)>1)) {
    vs <- resmca$var
    nlev <- sapply(resmca$call$X, nlevels)
    vnames <- names(resmca$call$X)
    long_names <- rownames(resmca$var$coord)
    short_names <- unlist(sapply(resmca$call$X, levels))
    variables <- character()
    for(i in 1:length(vnames)) variables <- c(variables, rep(vnames[i], nlev[i]))
    if(type %in% c("csMCA","speMCA","stMCA","multiMCA","bcMCA")) {
      variables <- variables[-resmca$call$excl]
      short_names <- short_names[-resmca$call$excl]
      }
    df <- data.frame(short_names,
                     long_names,
                     names = long_names,
                     vnames = factor(variables, levels = names(resmca$call$X)),
                     coord = vs$coord[,paste0("dim.",axis)],
                     freq = vs$weight,
                     cos2 = vs$cos2[,paste0("dim.",axis)],
                     ctr = vs$contrib[,paste0("dim.",axis)])
    
    if(!vlab) df$names <- df$short_names
    df$names <- paste0("'",df$names,"'")
    if(underline) {
      seuil <- 100/nrow(resmca$var$contrib)
      df$names[df$ctr>=seuil] <- paste0("underline(",df$names[df$ctr>=seuil],")")
    }
  }

  if(!is.null(min.ctr)) {
    if(min.ctr == "best") { 
      df <- df[df$ctr>=100/nrow(resmca$var$contrib),] 
    } else if(is.numeric(min.ctr)) {
      df <- df[df$ctr>=min.ctr,] 
    }
  }

  if(is.character(var) & length(var)>1) df <- df[df$vnames %in% var,]
  
  if(is.null(prop)) { 
    df$size = rep(1, times = nrow(df))
  } else if(prop=="freq") {
    df$size = df$freq
  } else if(prop=="cos2") {
    df$size = df$cos2
  } else if(prop=="ctr") {
    df$size = df$ctr
  } else if(prop=="pval") {
    df$size = 1-df$pval
  } else if(prop=="cor") {
    df$size = df$cor
  }
  
  p <- 
    ggplot2::ggplot(data = df) +
      ggplot2::geom_segment(x = min(df$coord)*1.1, y = 0, xend = max(df$coord)*1.1, yend = 0,
                   linewidth = .1, col = "darkgrey",
                   arrow = ggplot2::arrow(ends = "both", type = "closed", length = ggplot2::unit(0.1, "inches"))) +
      # geom_hline(yintercept = 0, colour = "darkgrey", linewidth = .1) +
      ggplot2::geom_point(x = 0, y = 0, colour = "darkgrey", size = ggplot2::rel(1))
  
  if(is.null(var)) {
    if(!is.null(col)) {
    p <- p + ggrepel::geom_text_repel(ggplot2::aes(x = .data$coord, y = 0, label = .data$names, size = .data$size), #, color = .data$vnames),
                                      direction = "y", segment.alpha = 0.3,
                                      force = force, 
                                      max.overlaps = max.overlaps,
                                      min.segment.length = 0, parse = TRUE,
                                      colour = col) #+
             #ggplot2::scale_color_manual(values = rep(col, length(vnames)))
    } else {
      p <- p + ggrepel::geom_text_repel(ggplot2::aes(x = .data$coord, y = 0, label = .data$names, size = .data$size, color = .data$vnames),
                                        direction = "y", segment.alpha = 0.3,
                                        force = force, 
                                        max.overlaps = max.overlaps, min.segment.length = 0, parse = TRUE)
    }
  } else {
    if(is.null(col)) col <- "black"
    p <- p + ggrepel::geom_text_repel(ggplot2::aes(x = .data$coord, y = 0, label = .data$names, size = .data$size),
                                      direction = "y", segment.alpha = 0.3,
                                      force = force, 
                                      max.overlaps = max.overlaps, min.segment.length = 0, parse = TRUE,
                                      colour = col)
  }
  
  minc <- min(df$coord)
  maxc <- max(df$coord)
  if(minc >= 0) { 
    breaks <- seq(from = 0, to = maxc, by = 0.5)
  } else if(maxc<=0) {
    breaks <- seq(from = 0, to = minc, by = -0.5)
  } else {
    breaks <- c(seq(from = 0, to = minc, by = -0.5), seq(from = 0, to = maxc, by = 0.5))
  }
  breaks <- sort(unique(breaks))
  breaks <- breaks[breaks > minc & breaks < maxc]
  breaks <- round(breaks, 1)

  p <- p +
    ggplot2::scale_x_continuous(breaks = breaks, 
                                limits = 1.2*c(minc,maxc),
                                name = paste("Coordinates on axis", axis)) +
    ggplot2::scale_size(limits = c(1, NA)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_text(margin = ggplot2::unit(c(5,0,0,0), units = "mm")),
                   aspect.ratio = 0.3,
                   legend.position = "none")
  
  return(p)
}
