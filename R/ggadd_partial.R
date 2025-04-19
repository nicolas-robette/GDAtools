ggadd_partial <- function(p, 
                          resmca,
                          var,
                          control,
                          sel = 1:nlevels(var),
                          axes = c(1,2),
                          col = "black",
                          textsize = 4,
                          lines = TRUE,
                          dashes = TRUE,
                          legend = "right",
                          force = 1, 
                          max.overlaps = Inf) {
  
  control <- as.data.frame(control)
  
  type <- attr(resmca,'class')[1]
  
  if(type=="bcMCA") {
    wt <- resmca$mycall$row.w
  } else if(type=="csMCA") {
    wt <- resmca$call$row.w[resmca$call$subcloud]
  } else {
    wt <- resmca$call$row.w
  }
  
  if(type=="stMCA") type <- resmca$call$input.mca
  if(type=="csMCA") {
    var <- var[resmca$call$subcloud]
    control <- control[resmca$call$subcloud,]
  }
  if(type=="multiMCA") {
    if(class(resmca$my.mca[[1]])[1]=="csMCA") {
      var <- var[resmca$my.mca[[1]]$call$subcloud]
      control <- control[resmca$my.mca[[1]]$call$subcloud,]
    }
  }

  var <- factor(var)
  control <- sapply(control, factor)
  
  wvar <- descriptio::weighted.table(var, weights = wt)
  
  ind <- as.data.frame(resmca$ind$coord)[,axes]
  
  coord_main <- agg.wtd.mean(ind, var, wt)
  coord_main$cat <- rownames(coord_main)
  coord_main$type <- rep("main", nrow(coord_main))

  coord_partial <- 
    lapply(ind, function(x) data.frame(dim = x, var, control) |>
                            lm(dim ~ . - 1, weights = wt, data = _) |>
                            coef()) |>
    do.call("rbind", args = _) |>
    t() |>
    as.data.frame()
  coord_partial <- coord_partial[1:nlevels(var),]
  coord_partial$cat <- levels(var)
  coord_partial$type <- rep("partial", nrow(coord_partial))
  coord_partial[,1] <- coord_partial[,1] - weighted.mean(coord_partial[,1], wvar)
  coord_partial[,2] <- coord_partial[,2] - weighted.mean(coord_partial[,2], wvar)

  coord <- rbind.data.frame(coord_main, coord_partial)
  names(coord)[1:2] <- c('axeX','axeY')
  coord <- coord[coord$cat %in% levels(var)[sel],]
  coord$cat <- factor(coord$cat)
  coord$type <- factor(coord$type)

  p <- p + ggplot2::geom_point(data = coord, 
                               ggplot2::aes(alpha = .data$type),
                               color = col) +
    ggrepel::geom_text_repel(key_glyph = 'blank', 
                             data = coord, 
                             ggplot2::aes(alpha = .data$type,
                                          label = .data$cat),
                             color = col,
                             size = textsize,
                             force = force, 
                             max.overlaps = max.overlaps) +
    ggplot2::scale_alpha_discrete("effect", range = c(1, 0.3))
  
  if(lines) p <- p +
    ggplot2::geom_path(data=coord,
                       ggplot2::aes(x = .data$axeX,
                                    y = .data$axeY,
                                    alpha = .data$type,
                                    group = .data$type),
                       color = col)
  
  if(dashes) p <- p +
    ggplot2::geom_path(data = coord, 
                       ggplot2::aes(x = .data$axeX,
                                    y = .data$axeY,
                                    group = .data$cat),
                       color = "darkgray", 
                       linetype = "dashed")
  
  p <- p + 
    ggplot2::theme(legend.position = legend)
  
  p
}
