# resmca = speMCA(Taste[,1:11], excl=c(3,6,9,12,15,18,21,24,27,30,33))
# var = Taste$Educ
# var = "Classical"
# var = NULL
# axis = 3
# 
# ggaxis_variables(resmca, var = NULL, axis = 1, prop = NULL, palette = "khroma::bright")
# ggaxis_variables(resmca, var = NULL, axis = 1, prop = NULL, palette = NULL, color = "orange")
# ggaxis_variables(resmca, var = NULL, axis = 1, prop = "ctr")
# ggaxis_variables(resmca, var = NULL, axis = 1, prop = "freq", underline = TRUE)
# ggaxis_variables(resmca, var = NULL, axis = 1, prop = "cos2")
# 
# ggaxis_variables(resmca, var = "Classical", axis = 1, prop = "ctr", underline = TRUE)
# 
# ggaxis_variables(resmca, var = Taste$Educ, axis = 1, prop = "pval")

ggaxis_variables <- function(resmca, var = NULL, axis = 1, prop = NULL, underline = FALSE, 
                             color = "black", palette = "khroma::bright") {

  type <- attr(resmca,'class')[1]
  
  if(is.factor(var)) {
    vs <- varsup(resmca,var)
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
      df$names[df$ctr>seuil] <- paste0("underline(",df$names[df$ctr>seuil],")")
    }
  }
  
  if(is.null(var)) {
    vs <- resmca$var
    nlev <- sapply(resmca$call$X, nlevels)
    vnames <- names(resmca$call$X)
    variables <- character()
    for(i in 1:length(vnames)) variables <- c(variables, rep(vnames[i], nlev[i]))
    if(type %in% c("csMCA","speMCA","stMCA","multiMCA")) variables <- variables[-resmca$call$excl]
    df <- data.frame(names = names(vs$weight),
                     vnames = factor(variables, levels = names(resmca$call$X)),
                     coord = vs$coord[,paste0("dim.",axis)],
                     freq = vs$weight,
                     cos2 = vs$cos2[,paste0("dim.",axis)],
                     ctr = vs$contrib[,paste0("dim.",axis)])
    if(underline) {
      seuil <- 100/nrow(resmca$var$contrib)
      df$names[df$ctr>seuil] <- paste0("underline(",df$names[df$ctr>seuil],")")
    }
  }

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
                   arrow = arrow(ends = "both", type = "closed", length = unit(0.1, "inches"))) +
      # geom_hline(yintercept = 0, colour = "darkgrey", linewidth = .1) +
      ggplot2::geom_point(x = 0, y = 0, colour = "darkgrey", size = rel(1))
  
  if(is.null(var) & !is.null(palette)) {
    p <- p + ggrepel::geom_text_repel(ggplot2::aes(x = .data$coord, y = 0, label = .data$names, size = .data$size, color = .data$vnames),
                                      direction = "y", segment.alpha = 0.3, max.overlaps = Inf, min.segment.length = 0, parse = TRUE) +
             paletteer::scale_color_paletteer_d(palette = palette)
  } else {
    p <- p + ggrepel::geom_text_repel(ggplot2::aes(x = .data$coord, y = 0, label = .data$names, size = .data$size),
                                      direction = "y", segment.alpha = 0.3, max.overlaps = Inf, min.segment.length = 0, parse = TRUE,
                                      colour = color)
  }
  
  p <- p +
    ggplot2::xlim(c(min(df$coord)*1.1, max(df$coord)*1.1)) +
    ggplot2::xlab(paste("Coordinates on axis", axis)) +
    # coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_text(margin = unit(c(5,0,0,0), units = "mm")),
                   aspect.ratio = 0.1,
                   legend.position = "none")
  
  return(p)
  
}
