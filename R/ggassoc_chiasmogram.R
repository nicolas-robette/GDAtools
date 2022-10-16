ggassoc_chiasmogram <- function(data, mapping, measure = "phi", max.asso = NULL,
                                sort = "none", palette = "PRGn", direction = 1) {

  xVal <- rlang::eval_tidy(mapping$x, data)
  yVal <- rlang::eval_tidy(mapping$y, data)
  xName <- rlang::as_name(mapping$x)
  yName <- rlang::as_name(mapping$y)
  
  # if(sort=="none") df$var.y <- factor(df$var.y, levels=rev(levels(df$var.y)))
  if(sort!="none") {
    temp <- MASS::corresp(~xVal+yVal,nf=1)
    if(sort %in% c("x","both")) xVal <- factor(xVal, levels=names(sort(temp$rscore)))
    if(sort %in% c("y","both")) yVal <- factor(yVal, levels=names(sort(temp$cscore)))
  }
  
  breaks1 <- as.data.frame(table(xVal))
  breaks1$cumw <- cumsum(breaks1$Freq)
  breaks1$pos1 <- .5 * (breaks1$cumw + lag(breaks1$cumw, default = 0))
  breaks1$prop1 = round(100*breaks1$Freq/sum(breaks1$Freq),1)
  breaks1$var.xb = paste(breaks1$prop1, breaks1$xVal, sep = " - ")
  # breaks1 <- breaks1[,c("xVal","pos1","Freq","prop1","var.xb")]
  names(breaks1)[1] <- "var.x"
  names(breaks1)[2] <- "freq1"
  breaks1$cumw <- NULL
  
  breaks2 <- as.data.frame(table(yVal))
  breaks2$cumw <- cumsum(breaks2$Freq)
  breaks2$pos2 <- .5 * (breaks2$cumw + lag(breaks2$cumw, default = 0))
  breaks2$prop2 = round(100*breaks2$Freq/sum(breaks2$Freq),1)
  breaks2$var.yb = paste(breaks2$prop2, breaks2$yVal, sep = " - ")
  # breaks2 <- breaks2[,c("yVal","pos2","Freq","prop2","var.yb")]
  names(breaks2)[1] <- "var.y"
  names(breaks2)[2] <- "freq2"
  breaks2$cumw <- NULL
  
  df <- GDAtools::assoc.twocat(xVal, yVal)$gather
  df <- merge(df, breaks1, by = "var.x")
  df <- merge(df, breaks2, by = "var.y")

  df$asso <- df[,measure]
  
  if(is.null(max.asso)) max.asso <- max(abs(df$asso))*1.1
  
  ggplot2::ggplot(df, ggplot2::aes(x = .data$pos1, y = .data$pos2)) +
    ggplot2::geom_tile(aes(width = .data$freq1,
                           height = .data$freq2,
                           fill = .data$asso),
                       color = 'black') +
    ggplot2::scale_x_continuous(breaks = breaks1$pos1, labels = breaks1$var.xb, expand = c(0, 0.1), position = "top") +
    ggplot2::scale_y_continuous(breaks = breaks2$pos2, labels = breaks2$var.yb, expand = c(0, 0.1), position = "right") +
    ggplot2::scale_fill_distiller(palette = palette, direction = direction, limits=c(-max.asso, max.asso),
                                  name = measure) +
    ggplot2::xlab(xName) +
    ggplot2::ylab(yName) +
    ggplot2::theme(axis.text.x = element_text(angle = 45, vjust = 0, hjust = 0),
                   legend.position = "bottom",
                   legend.key.size = unit(1, 'cm'))
}