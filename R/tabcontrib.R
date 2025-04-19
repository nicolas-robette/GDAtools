tabcontrib <- function(resmca, dim = 1, best = TRUE, limit = NULL, dec = 2, shortlabs = FALSE) {
  
  # initial data frame of contributions
  df1 <- data.frame(varcat = names(resmca$var$weight), weight = resmca$var$weight)
  df2 <- data.frame(varcat = rownames(resmca$var$coord), coord = resmca$var$coord[,dim])
  df3 <- data.frame(varcat = rownames(resmca$var$contrib), ctr = resmca$var$contrib[,dim])
  df4 <- data.frame(varcat = rownames(resmca$var$cos2), cos2 = resmca$var$cos2[,dim])
  df <- merge(merge(merge(merge(getvarnames(resmca), df1, by = "varcat"), df2, by = "varcat"), df3, by = "varcat"), df4, by = "varcat")
  df$sign <- sign(df$coord)
  
  if(best & is.null(limit)) { 
    limit <- 100/nrow(df)
  } else if (isFALSE(best)) {
    limit <- 0
  } else {
    if(limit<0 | limit>100) stop("limit argument should be set between 0 and 100.")
  }
  
  df <- df[df$ctr >= limit,]
  
  # ctr by variable
  w <- aggregate(cbind(ctrtot = ctr) ~ var, data = df, FUN = sum)
  # w <- w[order(w$ctrtot, decreasing = TRUE),]
  w$rank <- rank(-w$ctrtot)
  w <- w[order(w$rank),]
  w$cumctr <- cumsum(w$ctrtot)
  
  # barycentres
  temp <- df
  temp$wsum <- temp$weight * temp$coord
  b1 <- aggregate(wsum ~ sign + var, data = temp, FUN = sum)
  b2 <- aggregate(cbind(totw = weight) ~ sign + var, data = temp, FUN = sum)
  bary <- merge(b1, b2, by = c("var", "sign"))
  bary$coord <- bary$wsum / bary$totw
  bary$wsum <- NULL
  
  # ctr of deviation
  d1 <- aggregate(cbind(nd = 1/totw) ~ var, data = bary, FUN = sum)
  d1$nd <- 1/d1$nd
  d2 <- aggregate(cbind(dev = abs(coord)) ~ var, data = bary, FUN = sum)
  devi <- merge(d1, d2, by = "var")
  n <- nrow(resmca$call$X)
  Q <- ncol(resmca$call$X)
  eig <- resmca$eig$eig[dim]
  devi$ctrdev <- devi$nd * devi$dev * devi$dev / (Q * n * eig)
  devi <- devi[, c("var", "ctrdev")]
  
  # proportion to question
  prop <- data.frame(var = rownames(resmca$var$v.contrib), ctrvar = resmca$var$v.contrib[,dim])
  prop <- merge(prop, devi, by = "var")
  prop$ctrvar <- 100 * prop$ctrdev / prop$ctrvar
  prop$ctrdev <- NULL
  
  # all together
  res <- merge(df, w, by = "var", all.x = TRUE, all.y = FALSE)
  res <- merge(res, devi, by = "var", all.x = TRUE, all.y = FALSE)
  res <- merge(res, prop, by = "var", all.x = TRUE, all.y = FALSE)
  res <- res[order(res$rank, -res$ctr),]
  res$count <- unlist(lapply(rle(as.vector(res$var))$lengths, function(x) {1:x}))
  res$cos2 <- round(res$cos2, 3)
  res$ctr2 <- res$ctr1 <- res$ctr
  res$ctr1 <- round(res$ctr1, dec)
  res$ctr2 <- round(res$ctr2, dec)
  res$ctrtot <- round(res$ctrtot, dec)
  res$cumctr <- round(res$cumctr, dec)
  res$ctrdev <- round(100*res$ctrdev, dec)
  res$ctrvar <- round(100*res$ctrvar, dec)
  res$ctr1[res$sign==1] <- ""
  res$ctr2[res$sign==-1] <- ""
  res$var[res$count>1] <- ""
  res$ctrtot[res$count>1] <- ""
  res$cumctr[res$count>1] <- ""
  res$ctrdev[res$count>1] <- ""
  res$ctrvar[res$count>1] <- ""
  res <- res[, c("var", "cat", "weight", "cos2", "ctr1", "ctr2", "ctrtot", "cumctr", "ctrdev", "ctrvar")]
  if(!shortlabs) names(res) <- c("Variable", "Category", "Weight", "Quality of representation","Contribution (negative side)", "Contribution (positive side)",
                                 "Total contribution", "Cumulated contribution", "Contribution of deviation", "Proportion to variable")
  return(res)
}
