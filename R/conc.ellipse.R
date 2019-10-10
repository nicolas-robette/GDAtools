conc.ellipse <- function(resmca,var,sel=1:length(levels(varb)),axes=c(1,2),col=rainbow(length(sel)),pcol=rainbow(length(sel)),pcex=0.2,lty=1,lwd=1,tcex=1,text.lab=TRUE) {
  m <- varsup(resmca,var)$coord[,axes]
  m[,1] <- m[,1]*resmca$svd$vs[axes[1]]
  m[,2] <- m[,2]*resmca$svd$vs[axes[2]]
  v <- varsup(resmca,var)$var[1:length(levels(var)),axes]
  classe <- class(resmca)[1]
  if(classe=='stMCA') classe=resmca$call$input.mca
  if(classe == 'csMCA') {
    #varb <- var[resmca$call$subcloud]
    if(length(var)==nrow(resmca$call$X)) varb <- var[resmca$call$subcloud]
    if(length(var)==length(resmca$call$marge.row)) varb <- var
    wt <-  resmca$call$row.w[resmca$call$subcloud]
  }
  if(classe %in% c('MCA','speMCA','multiMCA')) {
    varb <- var
    if(classe=='multiMCA') { # new
      if(length(var)==nrow(resmca$my.mca[[1]]$call$X)) varb <- var[resmca$my.mca[[1]]$call$subcloud] # new
    } # new
    wt <-  resmca$call$row.w
  }
  c <- vector(length=length(levels(var)))
  for(i in 1:length(c)) {
    temp1 <- matrix(resmca$ind$coord[varb==levels(varb)[i],axes],ncol=2)
    temp1[,1] <- temp1[,1] - m[i,1]
    temp1[,2] <- temp1[,2] - m[i,2]
    temp2 <- wt[varb==levels(varb)[i]]*temp1[,1]*temp1[,2]
    c[i] <- sum(temp2)/sum(wt[varb==levels(varb)[i]])
  }
  g1 <- 0.5*(v[,1]+v[,2])+0.5*sqrt((v[,1]-v[,2])^2+4*c^2)
  g2 <- 0.5*(v[,1]+v[,2])-0.5*sqrt((v[,1]-v[,2])^2+4*c^2)
  sa1 <- 2*sqrt(g1)
  sa2 <- 2*sqrt(g2)
  alph <- atan((g1-v[,1])/c)
  npoints <- 100
  theta <- seq(0, 2 * pi, length=(npoints))
  if(length(col)==1) col <- rep(col,length(sel)) #NEW
  if(length(pcol)==1) pcol <- rep(pcol,length(sel)) #NEW
  if(length(lty)==1) lty <- rep(lty,length(sel)) #NEW
  if(length(lwd)==1) lwd <- rep(lwd,length(sel)) #NEW  
  if(!(is.null(col)) & length(col)!=length(sel)) col <- rainbow(length(sel))
  if(!(is.null(pcol)) & length(pcol)!=length(sel)) pcol <- rainbow(length(sel)) #NEW
  for(i in 1:length(levels(varb))) {
    if(i %in% sel) {
      colprinc <- col[which(i==sel)]
      pcolprinc <- pcol[which(i==sel)] #NEW
      ltyprinc <- lty[which(i==sel)] #NEW
      lwdprinc <- lwd[which(i==sel)] #NEW
      x0 <- m[i,1]
      y0 <- m[i,2]
      alpha <- alph[i]
      a <- sa1[i]
      b <- sa2[i]
      x <- x0 + a * cos(theta) * cos(alpha) - b * sin(theta) * sin(alpha)
      y <- y0 + a * cos(theta) * sin(alpha) + b * sin(theta) * cos(alpha)
      #lines(x, y, type = "l", col=colprinc, lty=ltyprinc, lwd=lwdprinc)
      z1 <- x0 + c(a,b,a,b)*cos(alpha+c(0,pi/2,pi,3*pi/2))
      z2 <- y0 + c(a,b,a,b)*sin(alpha+c(0,pi/2,pi,3*pi/2))
      z <- cbind(z1,z2)
      lightcol=rgb(t(col2rgb(colprinc)),alpha=200,maxColorValue=255)
      lines(z[c(1,3),],col=lightcol,lty=2)
      lines(z[c(2,4),],col=lightcol,lty=2)
      #points(x0,y0,pch=19,cex=1,col=colprinc)
      points(resmca$ind$coord[varb==levels(varb)[i],axes],pch=19,cex=pcex,col=pcolprinc)
      if(text.lab) text(x0,y0+0.1,levels(varb)[i],col=colprinc,cex=tcex)
      lines(x, y, type = "l", col=colprinc, lty=ltyprinc, lwd=lwdprinc)
    }
  }
}
