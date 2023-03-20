DA <- function(data, class, row.w = NULL, type = "FR") {

  if(is.null(row.w)) row.w <- rep(1, nrow(data))
  row.w <- row.w * nrow(data) / sum(row.w)
  
  if(any(sapply(data, FUN = function(x) !is.numeric(x) & !is.integer(x)))) stop("variables in data should all be numeric or integer")
  if(!is.factor(class)) stop("class should be a factor")

  n <- sum(row.w)
  p <- ncol(data)
  K <- nlevels(factor(class))
  nk <- as.vector(descriptio::weighted.table(class, weights = row.w))
  names(nk) <- levels(class)
  pk <- nk/n
  
  X <- data
  Xk <- split(X, class)
  row.wk <- split(row.w, class)
  
  # V <- var(X)*(n-1)/n
  V <- descriptio::weighted.cov2(X, weights = row.w)
  R <- descriptio::weighted.cor2(X, weights = row.w)
  
  g <- matrix(apply(X, 2, stats::weighted.mean, w = row.w), ncol = 1)
  rownames(g) <- colnames(X)
  
  Vk <- list()
  Rk <- list()
  gk <- list()
  for(k in 1:K) {
    gk[[k]] <- matrix(apply(Xk[[k]], 2, stats::weighted.mean, w = row.wk[[k]]), ncol = 1)
    rownames(gk[[k]]) <- colnames(X)
    Vk[[k]] <- descriptio::weighted.cov2(Xk[[k]], weights = row.wk[[k]])
    Rk[[k]] <- descriptio::weighted.cor2(Xk[[k]], weights = row.wk[[k]])
  }
  names(Vk) <- levels(class)
  names(Rk) <- levels(class)
  names(gk) <- levels(class)
  
  W <- matrix(0, nrow = p, ncol = p)
  B <- matrix(0, nrow = p, ncol = p)
  for (k in 1:K) {
    W <- W + pk[k] * Vk[[k]]
    B <- B + pk[k] * (gk[[k]] - g) %*% t(gk[[k]] - g)
  }

  if (type=="FR") {
    res <- eigen(solve(V) %*% B)
    eig <- Re(res$values[1:(K-1)])
    UU <- Re(res$vectors[,1:(K-1)])
    normes <- sqrt(diag(t(UU) %*% V %*% UU))
    
  } else if (type=="GB") {
    res <- eigen(solve(W) %*% B)
    eig <- Re(res$values[1:(K-1)])
    W <- W * n / (n-K)  # estimation pour retrouver les résultats de LDA
    UU <- Re(res$vectors[,1:(K-1)])
    normes <- sqrt(diag(t(UU) %*% W %*% UU))
  }
    
  U <- sweep(UU, 2, normes, "/")
  rownames(U) <- colnames(X)
  colnames(U) <- paste("Dim", 1:ncol(U), sep = ".")

  Xcent <- X - rep(1, nrow(X)) %*% t(g)
  S <- as.matrix(Xcent) %*% U
  colnames(S) <- paste("Dim", 1:ncol(S), sep = ".")
  rownames(S) <- rownames(X)

  var <- list()
  var$coord <- U
  var$cor <- sapply(data.frame(S), function(y) sapply(Xcent, descriptio::weighted.cor, y = y, weights = row.w))
  var$cos2 <- matrix(1/(K-1), nrow = ncol(data), ncol = K-1)
  dimnames(var$cos2)  <- dimnames(var$coord)

  ind <- list()
  ind$coord <- S
  
  quali.sup <- list()
  centers <- sweep(t(as.matrix(dichotom(class))) %*% diag(row.w) %*% as.matrix(Xcent), 1, nk, "/")
  rownames(centers) <- levels(class)
  quali.sup$coord <- centers %*% U
  
  eig <- data.frame(eig, 
                    100*eig/sum(eig), 
                    100*cumsum(eig/sum(eig)))
  colnames(eig) <- c("eigenvalue", "percentage of variance", "cumulative percentage of variance")
  rownames(eig) <- paste("comp", 1:nrow(eig))
  eig <- as.matrix(eig)
  
  call <- list()
  call$row.w <- row.w / sum(row.w)
  call$col.w <- rep(1, ncol(X))
  call$scale.unit = TRUE
  call$ncp <- K-1
  call$centre <- apply(data, 2, stats::weighted.mean, w = row.w)
  call$ecart.type <- apply(data, 2, descriptio::weighted.sd, w = row.w)
  call$X <- cbind.data.frame(data, class)
  call$row.w.init <- row.w
  call$quali.sup <- list()
  call$quali.sup$quali.sup <- data.frame(class)
  call$quali.sup$modalite <- nlevels(class)
  call$quali.sup$nombre <- nk
  call$quali.sup$barycentre <- as.data.frame(sweep(t(as.matrix(dichotom(class))) %*% diag(row.w) %*% as.matrix(data), 1, nk, "/"))
  call$quali.sup$numero <- ncol(call$X)

  cor_ratio <- diag(B) / diag(V)
      
  res <- list(eig = eig, var = var, ind = ind, quali.sup = quali.sup, call = call, cor_ratio = cor_ratio)
  class(res) <- list("PCA", "AFD", "list")

  return(res)
}
