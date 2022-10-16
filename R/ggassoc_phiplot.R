ggassoc_phiplot <- function(data, mapping, measure="phi", max.asso=NULL, sort="none") {

  xVal <- rlang::eval_tidy(mapping$x, data)
  yVal <- rlang::eval_tidy(mapping$y, data)
  xName <- rlang::as_name(mapping$x)
  yName <- rlang::as_name(mapping$y)

  if(sort!="none") {
    temp <- MASS::corresp(~xVal+yVal,nf=1)
    if(sort %in% c("x","both")) xVal <- factor(xVal, levels=names(sort(temp$rscore)))
    if(sort %in% c("y","both")) yVal <- factor(yVal, levels=names(sort(temp$cscore)))
  }
  
  res0 <- assoc.twocat(xVal, yVal)
  res <- res0$gather
  
  res$sign = sign(res$phi)
  res$sign <- factor(res$sign)
  
  res$asso <- res[,measure]
  
  p <- ggplot2::ggplot(res, ggplot2::aes(x = 1, y = .data$asso, width = .data$prop.x, fill = .data$sign)) +
    ggplot2::geom_col(col = "black") +
    ggplot2::scale_fill_manual(values = c("white","black")) +
    ggplot2::facet_grid(.data$var.y ~ .data$var.x, scales = "free_x", space = "free") +
    xlab(xName) +
    ylab(measure) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   strip.text.y.right = ggplot2::element_text(angle = 0, hjust = 0))

  # ann_text <- data.frame(wm=10, w=-Inf, asso=Inf, var.y=factor(levels(res$var.y)[1], levels=levels(res$var.y)))
  # if(!is.null(text.size)) p <- p + ggplot2::geom_label(data=ann_text, ggplot2::aes(x=.data$wm, y=.data$asso, label=paste0("V = ",round(res0$cramer.v,3))),
  #                                                      size=text.size, hjust=0, vjust=1, label.size=NA, fill="white", alpha=.5)
  p
}
