
weighted.quantile <- function(x, w, probs = .5, method = "raw") {
  if(method=="raw") {
    w <- w[order(x)]
    x <- x[order(x)]
    Fx = cumsum(w)/sum(w)
    rang <- max(which(Fx<probs))
    res <- x[rang] + (0.5 - Fx[rang])/(Fx[rang+1] - Fx[rang]) * (x[rang+1] - x[rang])
  }
  if(method=="density") {
    res <- with(density(x, weights = w/sum(w), n = 4096), 
                x[which.max(cumsum(y*(x[2L] - x[1L])) >= probs)])
  }
  return(res)
}

weighted.mad <- function(x, w, method="raw") {
  med <- weighted.quantile(x=x, w=w, method=method)
  ad <- abs(x-med)
  mad <- weighted.quantile(x=ad, w=w, method=method)
  return(mad)
}

weighted.sd <- function(x, w) {
  xm <- weighted.mean(x, w)
  var <- weighted.mean((x-xm)^2, w)
  sd <- sqrt(var)
  return(sd)
}

lag1 <- function(x, default = 0) {
  return(c(default, x[1:(length(x)-1)]))
  }
