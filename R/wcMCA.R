wcMCA <- function(data, class, excl = NULL, row.w = NULL, ncp = 5) {

    if(is.null(row.w)) row.w <- rep(1, nrow(data))
    if(any(sapply(data, FUN = function(x) !is.factor(x)))) stop("variables in data should all be factors")
    if(!is.factor(class)) stop("class should be a factor")
    data <- as.data.frame(data)
    row.w <- row.w / sum(row.w) * nrow(data)
    if(is.character(excl)) excl <- which(getindexcat(data) %in% excl)
    if(is.null(excl)) excl <- 99999
    
    n <- sum(row.w)
    ncl <- tapply(row.w, class, sum)
    pcl <- as.vector(ncl / n)
    disj <- dichotom(data, out = "numeric")
    centers <- sweep(t(as.matrix(dichotom(class)))%*%diag(row.w)%*%as.matrix(disj), 1, ncl, "/")
    centers <- data.frame(centers)
    row.names(centers) <- levels(class)
    
    Q <- ncol(data)
    Z <- as.matrix(disj)
    K <- ncol(Z)
    eI <- matrix(rep(1,length=n),ncol=1)
    eK <- matrix(rep(1,length=K),ncol=1)
    # Z0 <- Z - (1/n)*crossprod(t(eI))%*%(row.w*Z)
    Z0 <- Z - as.matrix(centers[class, ])
    NK <- diag(colSums(row.w*Z))
    Z0t <- Z0[,-excl]
    NKp <- NK[-excl,-excl]
    H0t <- sqrt(row.w)*(1/sqrt(Q))*Z0t%*%diag(1/sqrt(colSums(row.w*Z)[-excl]))
    svd <- svd(H0t)
    dims <- paste('dim',1:ncp,sep='.')
    noms <- vector(length=ncol(Z))
    id=0
    for(i in 1:Q) {
      for(j in 1:length(levels(data[,i]))) {
        id=id+1
        noms[id] <- paste(colnames(data)[i],levels(data[,i])[j],sep='.')
      }}
    YIt <- (1/sqrt(row.w))*sqrt(n)*svd$u%*%diag(svd$d)
    YKpt <- sqrt(n*Q)*diag(1/sqrt(colSums(row.w*Z)[-excl]))%*%svd$v%*%diag(svd$d)
    eig <- list(svd$d*svd$d)
    eig[[2]] <- eig[[1]]/sum(eig[[1]])*100
    eig[[3]] <- cumsum(eig[[2]])
    seuil <- 1/Q
    e <- eig[[1]][eig[[1]]>=seuil]
    pseudo <- (Q/(Q-1)*(e-seuil))^2
    eig[[4]] <- round(pseudo/sum(pseudo)*100,2)
    eig[[5]] <- cumsum(eig[[4]])
    names(eig) <- c('eigen','rate','cum.rate','mrate','cum.mrate')
    weight=colSums(row.w*Z)
    coord <- YIt[,1:ncp]
    contrib <- 100*row.w/n*coord*coord/matrix(rep(eig[[1]][1:ncp],times=n),ncol=ncp,nrow=n,byrow=T)
    dimnames(coord) <- list(rownames(data),dims) 
    dimnames(contrib) <- list(rownames(data),dims) 
    ind <- list(coord=coord,contrib=round(contrib,6))
    coord <- YKpt[,1:ncp]
    fK <- colSums(row.w*Z)[-excl]/n
    contrib <- 100*(fK/Q)*coord*coord/matrix(rep(eig[[1]][1:ncp],times=ncol(Z0t)),ncol=ncp,nrow=ncol(Z0t),byrow=T)
    s <- vector()
    for(i in 1:Q) s <- c(s,rep(i,times=length(levels(data[,i]))))
    s <- s[-excl]
    v.contrib <- aggregate(contrib,list(s),sum)[,-1]
    dimnames(v.contrib) <- list(colnames(data),dims)
    ctr.cloud <- data.frame(100*(1-fK)/(ncol(Z0t)-Q))
    colnames(ctr.cloud) <- 'ctr.cloud'
    vctr.cloud <- aggregate(ctr.cloud,list(s),FUN=sum)[-1]
    dimnames(vctr.cloud) <- list(colnames(data),'vctr.cloud')
    cos2 <- coord*coord/((1/fK)-1)
    dimnames(coord) <- list(noms[-excl],dims)
    dimnames(contrib) <- list(noms[-excl],dims)
    dimnames(cos2) <- list(noms[-excl],dims)
    eta2 <- matrix(nrow=Q,ncol=ncp)
    for(j in 1:Q) eta2[j,] <- apply(ind$coord,2,function(x) summary(stats::lm(x~data[,j],weights=row.w))$r.squared)
    dimnames(eta2) <- list(colnames(data),dims)
    v.test <- sqrt(cos2)*sqrt(n-1)*(((abs(coord)+coord)/coord)-1)
    var <- list(weight=round(weight,1)[-excl],coord=coord,contrib=round(contrib,6),ctr.cloud=round(ctr.cloud,6),cos2=round(cos2,6),v.test=round(v.test,6),eta2=round(eta2,6),v.contrib=v.contrib,vctr.cloud=vctr.cloud)
    X <- data
    marge.col <- colSums(row.w*Z)[-excl]/(n*Q)
    names(marge.col) <- noms[-excl]
    marge.row <- rep(1/(n*Q),times=n)
    names(marge.row) <- 1:n
    quali <- 1:Q
    call <- list(X=X,marge.col=marge.col,marge.row=marge.row,ncp=ncp,quali=quali,excl=excl,excl.char=getindexcat(data)[excl],row.w=row.w)
    RES <- list(eig=eig,call=call,ind=ind,var=var,svd=list(vs=svd$d,U=svd$u,V=svd$v))
    mca <- speMCA(data = data, excl = excl, row.w = row.w, ncp = ncp)
    RES$ratio <- sum(RES$eig$eig) / sum(mca$eig$eig)
    attr(RES,'class') <- c('speMCA','wcMCA','list')
    return(RES)
}
