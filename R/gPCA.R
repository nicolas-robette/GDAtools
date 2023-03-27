gPCA <- function(X, row.w = NULL, col.w = NULL, center = FALSE, scale = FALSE, tol = 1e-07) {

  X <- data.frame(X)
  
  if(is.null(row.w)) row.w <- rep(1, nrow(X))
  row.w.init <- row.w
  row.w <- row.w / sum(row.w)
  if(is.null(col.w)) col.w <- rep(1, ncol(X))

  Xs <- X
  if(center) Xs <- data.frame(lapply(Xs, function(x) x - stats::weighted.mean(x,row.w)))
  if(scale) Xs <- data.frame(lapply(Xs, function(x) x / descriptio::weighted.sd(x,row.w)))
  
  Xe <- as.matrix(Xs)
  Xe <- Xe * sqrt(row.w)
  Xe <- sweep(Xe, 2, sqrt(col.w), "*")
  evd <- eigen(crossprod(Xe, Xe), symmetric = TRUE)
  
  eig <- evd$values
  rank <- sum((eig/eig[1]) > tol)
  eig <- eig[1:rank]
  c1 <- evd$vectors[, 1:rank] / sqrt(col.w)
  li <- sweep(as.matrix(Xs), 2, col.w, "*") %*% c1
  co <- sweep(c1, 2, sqrt(eig), "*")
  l1 <- sweep(li, 2, sqrt(eig), "/")
  
  svd <- list()
  svd$vs <- sqrt(eig)
  svd$U <- l1
  svd$V <- c1
  
  ind <- list()
  ind$coord <- li
  d2.ind <- rowSums(sweep(Xs*Xs, 2, col.w, "*"))
  ind$cos2 <- li * li / d2.ind
  ind$contrib <- 100*sweep(li*li*row.w/sum(row.w), 2, eig, "/")
  ind$dist <- sqrt(d2.ind)
  colnames(ind$coord) <- colnames(ind$cos2) <- colnames(ind$contrib) <- paste("Dim", 1:ncol(ind$coord), sep=".")
  rownames(ind$coord) <- rownames(ind$cos2) <- rownames(ind$contrib) <- names(ind$dist) <- rownames(X)
  
  var <- list()
  var$coord <- co
  d2.var <- as.vector(crossprod(rep(1, nrow(Xs)), as.matrix(Xs*Xs*row.w)))
  var$cor <- co / sqrt(d2.var)
  var$cos2 <- co * co / d2.var
  var$contrib <- 100*sweep(co*co*col.w, 2, eig, "/")
  var$dist <- sqrt(d2.var)
  colnames(var$coord) <- colnames(var$cor) <- colnames(var$cos2) <- colnames(var$contrib) <- paste("Dim", 1:ncol(var$coord), sep=".")
  rownames(var$coord) <- rownames(var$cor) <- rownames(var$cos2) <- rownames(var$contrib) <- names(var$dist) <- names(X)
  
  eig <- data.frame(eig, 
                    100*eig/sum(eig), 
                    100*cumsum(eig/sum(eig)))
  eig <- as.matrix(eig)
  colnames(eig) <- c("eigenvalue", "percentage of variance", "cumulative percentage of variance")
  rownames(eig) <- paste("comp", 1:nrow(eig))
  
  call <- list()
  call$row.w <- row.w
  call$col.w <- col.w
  call$center <- center
  call$scale.unit <- scale
  call$ncp <- rank
  call$rank <- rank
  if (isTRUE(center)) {
    call$centre <- sapply(X, function(x) weighted.mean(x,row.w))
  } else {
    call$centre <- rep(0, times = ncol(X))
  }
  if (isTRUE(scale)) {
    call$ecart.type <- sapply(X, function(x) descriptio::weighted.sd(x,row.w)) 
  } else {
    call$ecart.type <- rep(1, times = ncol(X))
    }
  call$X <- X
  call$row.w.init <- row.w.init
  call$call <- match.call()
  
  res <- list(eig = eig,
              var = var,
              ind = ind,
              svd = svd,
              call = call)
  class(res) <- list("PCA", "gPCA", "list")
  
  return(res)
}

