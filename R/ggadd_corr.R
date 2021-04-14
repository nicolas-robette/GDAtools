ggadd_corr <- function(p, resmca, var, cat=levels(var)[1], axes=c(1,2),
                       xbins=20, ybins=20, min.n=1, pal="RdYlBu", limits=NULL) {

  df <- as.data.frame(resmca$ind$coord[,axes])
  names(df) <- c("dim.1","dim.2")
  if(is.numeric(var)) df$vsup <- var
  if(is.factor(var)) df$vsup <- as.numeric(var==cat)

  gb <- ggplot2::ggplot_build(ggplot2::ggplot(data=df, ggplot2::aes(x=.data$dim.1, y=.data$dim.2)))
  # xrange <- ggp$layout$panel_scales_x[[1]]$range$range  # data range!
  # yrange <- ggp$layout$panel_scales_y[[1]]$range$range  # data range!
  xmin = gb$layout$panel_params[[1]]$x.range[1]
  xmax = gb$layout$panel_params[[1]]$x.range[2]
  ymin = gb$layout$panel_params[[1]]$y.range[1]
  ymax = gb$layout$panel_params[[1]]$y.range[2]
  xsize = (xmax-xmin)/xbins
  ysize = (ymax-ymin)/ybins
  
  df$pt_dim1 <- cut(df$dim.1, seq(from=xmin, to=xmax, by=xsize))
  df$pt_dim2 <- cut(df$dim.2, seq(from=ymin, to=ymax, by=ysize))
  levels(df$pt_dim1) <- seq(from=xmin+xsize/2, to=xmax-xsize/2, by=xsize)
  levels(df$pt_dim2) <- seq(from=ymin+ysize/2, to=ymax-ysize/2, by=ysize)
  
  temp <- with(df, assoc.catcont(interaction(pt_dim1,pt_dim2),vsup,nperm=NULL)$cor.coeff)
  temp <- data.frame(cell=factor(names(temp)), cor=temp, stringsAsFactors=FALSE)
  temp <- temp[!is.na(temp$cor),]
  
  df <- aggregate(vsup~pt_dim1+pt_dim2, data=df, FUN=function(x) c(mean=mean(x,na.rm=TRUE), n=length(x)))
  df <- do.call(data.frame,df)
  df$cell <- interaction(df$pt_dim1,df$pt_dim2)
  df <- merge(df,temp,by="cell",sort=FALSE)
  df$pt_dim1 <- as.numeric(as.character(df$pt_dim1))
  df$pt_dim2 <- as.numeric(as.character(df$pt_dim2))
  
  df <- df[df$vsup.n>=min.n,]
  
  if(is.null(limits)) limits <- c(-1,1)*max(abs(df$cor))
  
  p <- p + ggplot2::geom_tile(data=df, ggplot2::aes(x=.data$pt_dim1, y=.data$pt_dim2, fill=.data$cor)) +
           ggplot2::scale_fill_distiller(type="div", palette=pal, limits=limits)
  
  return(p)
}