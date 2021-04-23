ggassoc_boxplot <- function(data, mapping, axes.labs=TRUE, ticks.labs=TRUE, text.size=3, box=TRUE, notch=FALSE, violin=TRUE) {
  xVal <- GGally::eval_data_col(data, mapping$x)
  yVal <- GGally::eval_data_col(data, mapping$y)
  xName <- rlang::as_name(mapping$x)
  yName <- rlang::as_name(mapping$y)
  if(is.numeric(yVal)) {
    assoc <- round(summary(stats::lm(yVal ~ factor(xVal)))$adj.r.squared,3)
  } else {
    assoc <- round(summary(stats::lm(xVal ~ factor(yVal)))$adj.r.squared,3)
  }
  newdata <- data.frame(xVal,yVal)
  if(is.factor(newdata$yVal)) newdata$yVal <- factor(newdata$yVal, levels=rev(levels(newdata$yVal)))
  p <- ggplot2::ggplot(newdata, ggplot2::aes(x=.data$xVal, y=.data$yVal))
  
  if(violin) p <- p + ggplot2::geom_violin(scale="count", alpha=.6, color="grey")
  
  if(box) p <- p + ggplot2::geom_boxplot(varwidth=TRUE, notch=notch, fill="grey", alpha=.6, outlier.size=0.5, outlier.alpha=0.2)
  
  p <- p + 
          ggplot2::theme_minimal() +
          ggplot2::theme( legend.position="none",
                          panel.grid.major = ggplot2::element_blank(),
                          panel.grid.minor=ggplot2::element_blank(),
                          panel.border = ggplot2::element_rect(
                            linetype = "solid",
                            color = "grey",
                            fill = "transparent"))
  
  if(!is.null(text.size)) p <- p + ggplot2::annotate(geom="label", label=paste0("eta2 = ",round(assoc,3)), x=-Inf, y=Inf, size=text.size,
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