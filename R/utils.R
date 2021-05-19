StatChull <- ggproto("StatChull", Stat,
                     compute_group = function(data, scales) {
                       data[chull(data$x, data$y), , drop = FALSE]
                     },
                     
                     required_aes = c("x", "y")
)

stat_chull <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatChull, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

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
