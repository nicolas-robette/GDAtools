ggassoc_crosstab <- function(data, mapping, max.phi=.8, sort="none", axes.labs=TRUE, ticks.labs=TRUE, text.size=3) {
  xVal <- GGally::eval_data_col(data, mapping$x)
  yVal <- GGally::eval_data_col(data, mapping$y)
  xName <- rlang::as_name(mapping$x)
  yName <- rlang::as_name(mapping$y)
  assoc <- GDAtools::assoc.twocat(xVal, yVal, na_value=NULL, nperm=NULL)
  newdata <- assoc$gather
  if(sort=="none") newdata$Var2 <- factor(newdata$Var2, levels=rev(levels(newdata$Var2)))
  if(sort!="none") {
    temp <- MASS::corresp(~xVal+yVal,nf=1)
    if(sort %in% c("x","both")) newdata$Var1 <- factor(newdata$Var1, levels=names(sort(temp$rscore)))
    if(sort %in% c("y","both")) newdata$Var2 <- factor(newdata$Var2, levels=names(sort(temp$cscore)))
  }
  p <- GGally::ggally_count(newdata, ggplot2::aes(x=.data$Var1, y=.data$Var2, weight=.data$Freq, fill=.data$phi), col='black') +
          ggplot2::scale_fill_gradient2(low="darkred", high="black", midpoint=0, limits=c(-max.phi, max.phi)) +
          ggplot2::theme_minimal() +
          ggplot2::theme( #legend.position="none",
                          panel.grid.minor = ggplot2::element_blank(),
                          panel.border = ggplot2::element_rect(
                            linetype = "solid",
                            color = "grey",
                            fill = "transparent"))
  
  if(!is.null(text.size)) p <- p + ggplot2::annotate(geom='label', label=paste0("V = ",round(assoc$cramer.v,3)), x=-Inf, y=Inf, size=text.size,
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
