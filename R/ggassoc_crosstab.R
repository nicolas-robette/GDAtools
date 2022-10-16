ggassoc_crosstab <- function(data, mapping, size = "observed", measure = "phi", max.asso = NULL, sort = "none", palette = "PRGn", direction = 1) {

  xVal <- rlang::eval_tidy(mapping$x, data)
  yVal <- rlang::eval_tidy(mapping$y, data)
  xName <- rlang::as_name(mapping$x)
  yName <- rlang::as_name(mapping$y)
  
  if(sort!="none") {
    temp <- MASS::corresp(~xVal+yVal,nf=1)
    if(sort %in% c("x","both")) xVal <- factor(xVal, levels=names(sort(temp$rscore)))
    if(sort %in% c("y","both")) yVal <- factor(yVal, levels=names(sort(temp$cscore)))
  }
  
  res0 <- GDAtools::assoc.twocat(xVal, yVal)
  res <- res0$gather
  
  res$asso <- res[,measure]
  
  if(is.null(max.asso)) max.asso <- max(abs(res$asso))*1.1
  
  if(size=="observed") res$size <- res$freq
  if(size=="expected") res$size <- res$expected

  ggplot2::ggplot(res, aes(size = sqrt(.data$size), color = .data$asso, x = 1, y = 1)) +
    ggplot2::geom_count(shape = "square") +
    ggplot2::scale_size(range = c(0,50), guide = "none") +
    ggplot2::scale_color_distiller(palette = palette, limits = c(-max.asso, max.asso), name = measure) +
    ggplot2::facet_grid(.data$var.y ~ .data$var.x, scales = "free_x") + #, space = "free") +
    xlab(xName) +
    ylab(yName) +
    ggplot2::theme_minimal() +
    ggplot2::theme(# legend.position = "none",
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   strip.text.y.right = ggplot2::element_text(angle = 0, hjust = 0),
                   legend.position = "bottom",
                   legend.key.size = unit(0.5, 'cm'))
    
  # if(!is.null(text.size)) p <- p + ggplot2::annotate(geom='label', label=paste0("V = ",round(assoc$cramer.v,3)), x=-Inf, y=Inf, size=text.size,
  #                                                    hjust=0, vjust=1, label.size=NA, fill="white", alpha=.5)
}
