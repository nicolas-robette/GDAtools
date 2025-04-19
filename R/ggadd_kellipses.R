ggadd_kellipses <- function(p, resmca, var, sel=1:nlevels(var), axes=c(1,2), kappa=2, label=TRUE, label.size=3, size=0.5, points=TRUE, legend="right") {

  subvar <- factor(var)
  
  type <- attr(resmca,'class')[1]
  
  if(type=="bcMCA") {
    wt <- resmca$mycall$row.w
  } else {
    wt <- resmca$call$row.w
  }

  if(type=="stMCA") type <- resmca$call$input.mca
  if(type=="csMCA") {
    subvar <- factor(var[resmca$call$subcloud])
    wt <- resmca$call$row.w[resmca$call$subcloud]
  }
  if(type=="multiMCA") {
    if(class(resmca$my.mca[[1]])[1]=="csMCA") subvar <- var[resmca$my.mca[[1]]$call$subcloud]
  }
  
  m <- agg.wtd.mean(resmca$ind$coord[,axes], subvar, wt)
  v <- agg.wtd.var(resmca$ind$coord[,axes], subvar, wt)
  
  c <- vector(length=nlevels(subvar))
  for(i in 1:length(c)) {
    temp1 <- matrix(resmca$ind$coord[subvar==levels(subvar)[i],axes],ncol=2)
    temp1[,1] <- temp1[,1] - m[i,1]
    temp1[,2] <- temp1[,2] - m[i,2]
    temp2 <- wt[subvar==levels(subvar)[i]]*temp1[,1]*temp1[,2]
    c[i] <- sum(temp2)/sum(wt[subvar==levels(subvar)[i]])
  }
  g1 <- 0.5*(v[,1]+v[,2])+0.5*sqrt((v[,1]-v[,2])^2+4*c^2)
  g2 <- 0.5*(v[,1]+v[,2])-0.5*sqrt((v[,1]-v[,2])^2+4*c^2)
  sa1 <- kappa*sqrt(g1)
  sa2 <- kappa*sqrt(g2)
  # sa2 <- kappa*sqrt(abs(g2))
  alph <- atan((g1-v[,1])/c)
  npoints <- 100
  theta <- seq(0, 2 * pi, length=(npoints))

  cent <- list()
  ell <- list()
  rad1 <- list()
  rad2 <- list()
  for(i in 1:nlevels(subvar)) {
    if(i %in% sel) {
      x0 <- m[i,1]
      y0 <- m[i,2]
      cent[[i]] <- data.frame(cat=levels(subvar)[i],x=x0,y=y0)
      alpha <- alph[i]
      a <- sa1[i]
      b <- sa2[i]
      x <- x0 + a * cos(theta) * cos(alpha) - b * sin(theta) * sin(alpha)
      y <- y0 + a * cos(theta) * sin(alpha) + b * sin(theta) * cos(alpha)
      ell[[i]] <- data.frame(cat=rep(levels(subvar)[i],length(x)),x=x,y=y)
      z1 <- x0 + c(a,b,a,b)*cos(alpha+c(0,pi/2,pi,3*pi/2))
      z2 <- y0 + c(a,b,a,b)*sin(alpha+c(0,pi/2,pi,3*pi/2))
      rad1[[i]] <- data.frame(cat=rep(levels(subvar)[i],length(z1)),x=z1,y=z2)[c(1,3),]
      rad2[[i]] <- data.frame(cat=rep(levels(subvar)[i],length(z1)),x=z1,y=z2)[c(2,4),]
  }}
  ell <- do.call('rbind.data.frame',ell)
  rad1 <- do.call('rbind.data.frame',rad1)
  rad2 <- do.call('rbind.data.frame',rad2)
  cent <- do.call('rbind.data.frame',cent)
  ell$cat <- factor(ell$cat, levels = levels(subvar)[sel])
  rad1$cat <- factor(rad1$cat, levels = levels(subvar)[sel])
  rad2$cat <- factor(rad2$cat, levels = levels(subvar)[sel])
  cent$cat <- factor(cent$cat, levels = levels(subvar)[sel])
  
  pfin <- p + ggplot2::geom_path(data=ell, ggplot2::aes(x = .data$x, y = .data$y, color = .data$cat), linewidth=size) +
              ggplot2::geom_line(data=rad1, ggplot2::aes(x = .data$x, y = .data$y, color = .data$cat), lty=2, linewidth=0.3, alpha=0.5) +
              ggplot2::geom_line(data=rad2, ggplot2::aes(x = .data$x, y = .data$y, color = .data$cat), lty=2, linewidth=0.3, alpha=0.5)

  if(points) {
    icoord <- as.data.frame(resmca$ind$coord[,axes])
    names(icoord) <- c('x','y')
    icoord$cat <- subvar
    icoord <- icoord[subvar %in% levels(subvar)[sel],]
    icoord$cat <- factor(icoord$cat)
    pfin <- pfin + ggplot2::geom_point(data=icoord, aes(x = .data$x, y = .data$y, colour = .data$cat), size=0.5, alpha=0.6)
  }
  
  if(label) { 
    pfin <- pfin + ggplot2::geom_text(key_glyph='blank', data=cent, ggplot2::aes(x = .data$x, y = .data$y, label = .data$cat, colour = .data$cat), size=label.size) 
  } else { 
    pfin <- pfin + ggplot2::geom_point(data=cent, ggplot2::aes(x = .data$x, y = .data$y, colour = .data$cat), shape=8, size=3)
  }

  pfin <- pfin + ggplot2::guides(color = ggplot2::guide_legend(title = "")) +
                 ggplot2::theme(legend.position = legend)

  pfin
}
