DAQ <- function(data, class, excl = NULL, row.w = NULL, type = "FR", select = TRUE) {
  
  if(is.null(row.w)) row.w <- rep(1, nrow(data))
  row.w <- row.w * nrow(data) / sum(row.w)
  
  if(any(sapply(data, FUN = function(x) !is.factor(x)))) stop("variables in data should all be factors")
  if(!is.factor(class)) stop("class should be a factor")
  
  if(is.character(excl)) excl <- which(getindexcat(data) %in% excl)
  tdc <- dichotom(data)
  if(!is.null(excl)) tdc <- tdc[, -excl]

  mca0 <- speMCA(data, excl = excl)
  ncomp <- sum(mca0$eig$eigen > 1e-10)
  mca1 <- speMCA(data, excl = excl, ncp = ncomp)
  newdata <- as.data.frame(mca1$ind$coord)
  if(select) newdata <- newdata[,1:length(mca1$eig$mrate)]

  n <- sum(row.w)
  p <- ncol(newdata)
  K <- nlevels(factor(class))
  nk <- as.vector(descriptio::weighted.table(class, weights = row.w))
  names(nk) <- levels(class)
  pk <- nk/n
  
  X <- newdata
  Xk <- split(X, class)
  row.wk <- split(row.w, class)
  
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
  var$cos2 <- matrix(1/(K-1), nrow = nrow(var$coord), ncol = ncol(var$coord))
  dimnames(var$cos2)  <- dimnames(var$coord)

  ind <- list()
  ind$coord <- S

  quali.sup <- list()
  centers <- sweep(t(as.matrix(dichotom(class))) %*% diag(row.w) %*% as.matrix(Xcent), 1, nk, "/")
  rownames(centers) <- levels(class)
  quali.sup$coord <- centers %*% U
  
  quanti.sup <- list()
  centers2 <- sweep(t(as.matrix(tdc)) %*% diag(row.w) %*% as.matrix(Xcent), 1, colSums(tdc), "/")
  rownames(centers2) <- colnames(tdc)
  quanti.sup$coord <- centers2 %*% U
  quanti.sup$cor <- sapply(data.frame(S), function(y) sapply(tdc, descriptio::weighted.cor, y = y, weights = row.w))
  quanti.sup$cos2 <- matrix(1/ncol(quanti.sup$coord), nrow = nrow(quanti.sup$coord), ncol = ncol(quanti.sup$coord))
  dimnames(quanti.sup$cos2) <- dimnames(quanti.sup$coord)

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
  call$centre <- apply(newdata, 2, stats::weighted.mean, w = row.w)
  call$ecart.type <- apply(newdata, 2, descriptio::weighted.sd, w = row.w)
  call$X <- cbind.data.frame(newdata, class, tdc)
  call$row.w.init <- row.w
  call$quali.sup <- list()
  call$quali.sup$quali.sup <- data.frame(class)
  call$quali.sup$modalite <- nlevels(class)
  call$quali.sup$nombre <- nk
  call$quali.sup$barycentre <- as.data.frame(sweep(t(as.matrix(dichotom(class))) %*% diag(row.w) %*% as.matrix(tdc), 1, nk, "/"))
  call$quali.sup$numero <- ncol(newdata)+1
  call$quanti.sup <- tdc
  
  cor_ratio <- diag(B) / diag(V)
  
  res <- list(eig = eig, var = var, ind = ind, quali.sup = quali.sup, quanti.sup = quanti.sup, call = call, mca = mca1, cor_ratio = cor_ratio)
  class(res) <- list("PCA", "disqual", "AFD", "list")
  
  return(res)
}
