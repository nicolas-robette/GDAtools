ggassoc_assoplot <- function(data, mapping, measure="residuals", max.asso=NULL, sort="none", palette = "PRGn", direction = 1) {

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
  
  res$sign <- sign(res$phi)
  res$sign <- factor(res$sign)
  
  res$asso <- res[,measure]
  
  if(is.null(max.asso)) max.asso <- max(abs(res$asso))*1.1
  
  ggplot2::ggplot(res, ggplot2::aes(x = 1, y = .data$asso, width = sqrt(.data$expected), fill = .data$asso)) +
    ggplot2::geom_col(col = NA) +
    ggplot2::geom_hline(yintercept = 0, color = "gray70", lty = "dashed", lwd = rel(0.2)) +
    ggplot2::scale_fill_distiller(palette = palette, direction = direction, limits=c(-max.asso, max.asso)) +
    ggplot2::facet_grid(.data$var.y ~ .data$var.x, scales = "free_x", space = "free") +
    ggplot2::xlab(xName) +
    ggplot2::ylab(measure) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.text.x = element_blank(),
                   strip.text.y.right = ggplot2::element_text(angle = 0, hjust = 0))
}

