ggassoc_scatter <- function(data, mapping, axes.labs=TRUE, ticks.labs=TRUE, text.size=3) {
  xVal <- GGally::eval_data_col(data, mapping$x)
  yVal <- GGally::eval_data_col(data, mapping$y)
  xName <- rlang::as_name(mapping$x)
  yName <- rlang::as_name(mapping$y)
  assoc <- stats::cor(xVal, yVal, method="kendall", use='complete.obs')
  p <- ggplot2::ggplot(data, mapping) +
          ggplot2::geom_point(alpha=.8, size=rel(0.5)) +
          ggplot2::geom_smooth(method="gam", se=FALSE, size=rel(.7)) + 
          ggplot2::theme_minimal() +
          ggplot2::theme(legend.position="none",
                         panel.grid.major=ggplot2::element_blank(),
                         panel.grid.minor=ggplot2::element_blank(),
                         panel.border = ggplot2::element_rect(
                           linetype = "solid",
                           color = "grey",
                           fill = "transparent"))
  
  if(!is.null(text.size)) p <- p + ggplot2::annotate(geom="label", label=paste0("tau = ",round(assoc,3)), x=-Inf, y=Inf, size=text.size,
                                                     hjust=0, vjust=1, label.size=NA, fill="white", alpha=.5)
  if(axes.labs) {
    p <- p + ggplot2::xlab(xName) + ggplot2::ylab(yName)
  } else {
    p <- p + ggplot2::xlab(NULL) + ggplot2::ylab(NULL)
  }
  if(!ticks.labs) p <- p + ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                                          axis.text.x = ggplot2::element_blank())
  p
}