ggadd_supind <- function(p, resmca, dfsup, axes=c(1,2), col="black", textsize=5, pointsize=2) {
  
  if(nrow(dfsup)==1){
    dt <- as.data.frame.matrix(t(supind(resmca, dfsup)$coord[, axes]))
    rownames(dt)<-rownames(supind(resmca, dfsup)$coord)
  }else{
    dt <- as.data.frame(supind(resmca, dfsup)$coord[, axes])
  }
  
  names(dt) <- c('axeX','axeY')
  dt$names <- rownames(dt)
  
  if(is.null(pointsize)) {
    pfin <- p +
              ggrepel::geom_text_repel(data=dt, ggplot2::aes(x=.data$axeX, y=.data$axeY, label=.data$names), col=col, size=textsize, max.overlaps=Inf)
  }
  if(!is.null(pointsize)) {
    pfin <- p +
              ggplot2::geom_point(data=dt, ggplot2::aes(x=.data$axeX, y=.data$axeY), col=col, size=pointsize) +
              ggrepel::geom_text_repel(data=dt, ggplot2::aes(x=.data$axeX, y=.data$axeY, label=.data$names), col=col, size=textsize, max.overlaps=Inf)
  }
  pfin
}
