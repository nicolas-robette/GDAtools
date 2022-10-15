oddsratio.table <- function(x,y,weights=rep(1,length(x)),digits=3) {
  t0 <- GDAtools::wtable(x, y, weights, mar = FALSE)
  OR <- t0
  for(i in 1:nrow(t0)) {
    for(j in 1:ncol(t0)) {
      a <- t0[i,j]
      b <- rowSums(t0)[i] - a
      c <- colSums(t0)[j] - a
      d <- sum(t0) - (a+b+c)
      OR[i,j] <- (a*d) / (b*c)
    }
  }
  if(!is.null(digits)) OR <- round(OR,digits)
  return(OR)
}