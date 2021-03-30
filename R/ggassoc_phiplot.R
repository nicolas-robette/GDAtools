ggassoc_phiplot <- function(data, mapping, maxphi=NULL, axes.labs=TRUE, ticks.labs=TRUE) {
  xVal <- GGally::eval_data_col(data, mapping$x)
  yVal <- GGally::eval_data_col(data, mapping$y)
  xName <- rlang::as_name(mapping$x)
  yName <- rlang::as_name(mapping$y)
  assoc <- GDAtools::assoc.twocat(xVal, yVal, na_value=NULL, nperm=NULL)
  df <- as.data.frame(assoc$freq)
  df <- df[df$Var1!="Sum" & df$Var2!="Sum",]
  tot <- stats::aggregate(df$Freq,list(df$Var1),sum)
  names(tot) <- c("Var1" ,"TotV1")
  df <- merge(df,tot,by="Var1",sort=FALSE)
  df$Freq=NULL
  df <- merge(df,as.data.frame(assoc$phi),by=c("Var1","Var2"))
  names(df)[names(df)=="Freq"] <- "phi"
  df <- df[order(df$Var2,df$Var1),]
  df$sign = factor(sign(df$phi))
  df$w <- stats::ave(df$TotV1, df$Var2, FUN=cumsum)
  df$wm = df$w-df$TotV1
  df$wt = df$wm+(df$w-df$wm)/2
  if(is.null(maxphi)) maxphi <- max(ceiling(10*abs(df$phi)))/10
  labs <- unique(df[,c("Var1","wt")])
  ann_text <- data.frame(wm=10, w=-Inf, phi=Inf, Var2=factor(levels(df$Var2)[1], levels=levels(df$Var2)))
  p <- ggplot2::ggplot(df, ggplot2::aes(xmin=.data$wm, xmax=.data$w, ymin=0, ymax=.data$phi, fill=.data$sign)) +
          ggplot2::geom_rect(col="black", size=rel(0.2)) +
          ggplot2::facet_grid(rows=ggplot2::vars(.data$Var2)) +
          ggplot2::scale_fill_manual(values=c("white","black")) +
          ggplot2::theme_minimal() +
          ggplot2::theme( legend.position = "none",
                          panel.grid.major = ggplot2::element_blank(),
                          panel.grid.minor = ggplot2::element_blank(),
                          strip.text.y.right = ggplot2::element_text(angle=0)) +
          ggplot2::scale_x_continuous(position="top", breaks=labs$wt, labels=labs$Var1) +
          ggplot2::ylim(c(-maxphi, maxphi)) +
          ggplot2::geom_label(data=ann_text, ggplot2::aes(x=.data$wm, y=.data$phi, label=paste0("V = ",round(assoc$cramer.v,3))),
                              size=3, hjust=0, vjust=1, label.size=NA, fill="white", alpha=.5)
  if(axes.labs) {
    p <- p + ggplot2::xlab(xName) + ggplot2::ylab(yName)
  } else {
    p <- p + ggplot2::xlab(NULL) + ggplot2::ylab(NULL)
  }
  if(!ticks.labs) p <- p + ggplot2::theme(strip.text.y.right = ggplot2::element_blank(),
                                          axis.text.y = ggplot2::element_blank(),
                                          axis.text.x = ggplot2::element_blank())
  p
}