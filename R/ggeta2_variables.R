ggeta2_variables <- function(resmca, axes = c(1,2)) {
  
  type <- attr(resmca,'class')[1]
  dim1 <- axes[1]
  dim2 <- axes[2]
  
  if(type %in% c("MCA","speMCA","csMCA")) {
    rate1 <- modif.rate(resmca)$modif$mrate[dim1]
    rate2 <- modif.rate(resmca)$modif$mrate[dim2]
  } else if(type %in% c("stMCA","multiMCA")) {
    rate1 <- modif.rate(resmca)$raw$rate[dim1]
    rate2 <- modif.rate(resmca)$raw$rate[dim2]
  }
  
  df <- data.frame(resmca$var$eta2)[, axes]
  names(df) <- c("dimx", "dimy")
  df$name <- rownames(df)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$dimx, y = .data$dimy)) +
        ggplot2::geom_point(alpha = .5) +
        ggrepel::geom_text_repel(ggplot2::aes(label = .data$name), max.overlaps = Inf) +
        ggplot2::xlab(paste0("dim ", dim1, " (", round(rate1,1), " %)")) +
        ggplot2::ylab(paste0("dim ", dim2, " (", round(rate2,1), " %)")) +
        ggplot2::xlim(c(0,1)) +
        ggplot2::ylim(c(0,1)) +
        ggplot2::theme_light() +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) 
  
  return(p)
}
