assoc.twocat <- function(x, y, weights=rep.int(1,length(x)), na_value=NULL, nperm=NULL, distrib="asympt") {

  # add na level
  if(!is.null(na_value)) {
    x <- factor(x, levels=c(levels(x), na_value))
    x[is.na(x)] <- na_value
    y <- factor(y, levels=c(levels(y), na_value))
    y[is.na(y)] <- na_value
  }
  
  # drop empty levels
  x <- factor(x)
  y <- factor(y)
  
  # remove obs with na
  idnona <- !is.na(x) & !is.na(y)
  X <- x[idnona]
  Y <- y[idnona]
  W <- weights[idnona]
    
  t <- tapply(W, list(X,Y), sum)
  
  # remplace les cases vides par des 0  
  t[is.na(t)] <- 0

  tab <- as.table(t)

  freq <- addmargins(tab)
  prop <- 400*prop.table(freq)
  rprop <- 100*apply(freq, 2, function(x) 2*x/rowSums(freq))
  cprop <- t(100*apply(freq, 1, function(x) 2*x/colSums(freq)))

  phi <- phi.table(X,Y,weights=W,digits=NULL)
  or <- oddsratio.table(X,Y,weights=W,digits=NULL)
  pem <- pem(X,Y,weights=W,digits=NULL)
  peml <- pem$peml
  pemg <- pem$pemg
  
  expected <- sapply(colSums(t), function(x) x*rowSums(t)/sum(t))
  chi.squared <- sum((t-expected)*(t-expected)/expected)
  cramer.v <- sqrt(chi.squared / (length(x)*(min(nrow(t),ncol(t))-1)))
  expected <- as.table(expected)
  
  # stdres <- (t-expected)/sqrt(expected)
  # stdres <- as.table(stdres)
  old.warn <- options()$warn
  options(warn = -1)
  temp <- stats::chisq.test(t)
  stdres <- as.table(temp$stdres)
  res <- as.table(temp$res)
  options(warn = old.warn)
  
  if(!is.null(nperm)) {
    h0distrib <- numeric()
    for(i in 1:nperm) {
      permt <- table(x,sample(y))
      permexp <- rowSums(permt) %*% t(colSums(permt)) / sum(permt)
      h0distrib[i] <- sum((permt-permexp)*(permt-permexp)/permexp)
    }
    if(distrib=='approx') {
      permutation.pvalue <- sum(chi.squared<=h0distrib)/nperm
    } else {
      fit <- MASS::fitdistr(h0distrib,"normal")$estimate
      permutation.pvalue <- 1-stats::pnorm(chi.squared,fit[1],fit[2])   
    }
  }
  if(is.null(nperm)) permutation.pvalue <- NULL

  if(!is.null(nperm)) {  
    ar <- array(NA, dim=c(nperm,nrow(phi),ncol(phi)))
    for(i in 1:nperm) ar[i,,] <- GDAtools::phi.table(x,sample(y),weights=weights,digits=NULL)
    ppval <- matrix(nrow=nrow(phi), ncol=ncol(phi))
    for(i in 1:nrow(ppval)) { 
      for(j in 1:ncol(ppval)) {
        h0 <- ar[,i,j]
        obs <- phi[i,j]
        if(distrib=='approx') {
          if(obs>=0) ppval[i,j] <- sum(obs<=h0)/nperm
          if(obs<0) ppval[i,j] <- sum(obs>h0)/nperm
        } else {
          fit <- MASS::fitdistr(h0,"normal")$estimate
          if(obs>=0) ppval[i,j] <- 1-stats::pnorm(obs,fit[1],fit[2])
          if(obs<0) ppval[i,j] <- stats::pnorm(obs,fit[1],fit[2])
        }
    }}
    ppval <- as.table(ppval)
    dimnames(ppval) <- dimnames(phi)
  }
  if(is.null(nperm)) ppval <- NULL
  
  dimnames(expected) <- dimnames(phi)
  dimnames(stdres) <- dimnames(phi)
  
  pij <- t/sum(t)
  pi <- rowSums(pij)
  pj <- colSums(pij)
  vx <- 1 - sum(pi^2)
  vy <- 1 - sum(pj^2)
  xyTerm <- apply(pij^2, MARGIN = 1, sum)
  vyBarx <- 1 - sum(xyTerm/pi)
  yxTerm <- apply(pij^2, MARGIN = 2, sum)
  vxBary <- 1 - sum(yxTerm/pj)
  tauxy <- (vy - vyBarx)/vy
  tauyx <- (vx - vxBary)/vx
  
  gather <- cbind.data.frame(data.frame(tab), 
                             prop=data.frame(prop.table(tab))$Freq,
                             rprop=data.frame(prop.table(tab,1))$Freq,
                             cprop=data.frame(prop.table(tab,2))$Freq,
                             expected=data.frame(expected)$Freq,
                             std.residuals=data.frame(res)$Freq,
                             adj.residuals=data.frame(stdres)$Freq,
                             or=data.frame(or)$Freq,
                             pem=data.frame(peml)$Freq,
                             phi=data.frame(phi)$Freq)
  
  if(!is.null(ppval)) gather <- cbind.data.frame(gather, perm.pval=data.frame(ppval)$Freq)
  
  names(gather)[1:3] <- c("var.x","var.y","freq")
  
  t1 <- data.frame(GDAtools::wtable(X, weights = W, mar= FALSE))
  names(t1) <- c("var.x","freq.x")
  t2 <- data.frame(GDAtools::wtable(Y, weights = W, mar= FALSE))
  names(t2) <- c("var.y","freq.y")
  t3 <- data.frame(prop.table(GDAtools::wtable(X, weights = W, mar= FALSE)))
  names(t3) <- c("var.x","prop.x")
  t4 <- data.frame(prop.table(GDAtools::wtable(Y, weights = W, mar= FALSE)))
  names(t4) <- c("var.y","prop.y")
  
  gather <- merge(gather, t1, by = "var.x")
  gather <- merge(gather, t2, by = "var.y")
  gather <- merge(gather, t3, by = "var.x")
  gather <- merge(gather, t4, by = "var.y")

  return(list('tables' = list('freq'=freq,
                              'prop'=prop, 
                              'rprop'=rprop, 
                              'cprop'=cprop, 
                              'expected'=expected),
              'global' = list('chi.squared'=chi.squared,
                              'cramer.v'=cramer.v,
                              'permutation.pvalue'=permutation.pvalue,
                              'global.pem'=pemg,
                              'GK.tau.xy'=tauxy,
                              'GK.tau.yx'=tauyx),
              'local' = list('std.residuals'=res,
                             'adj.residuals'=stdres,
                             'odds.ratios'=or,
                             'local.pem'=peml,
                             'phi'=phi,
                             'phi.perm.pval'=ppval),
              'gather'=gather))
}

# data(Music)
# assoc.twocat(Music$Jazz,Music$Age,nperm=100)
# x = Music$Jazz
# y = Music$Age
# weights=rep.int(1,length(x))
# 
# library(GoodmanKruskal)
# GKtau(x, y, dgts = 5)
