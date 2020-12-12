translate.logit <- function(formula,data,nit=0) {

  #z <- installed.packages()
  #if(sum(z[,1] == "nleqslv") == 0) stop("nleqslv package not installed")

  #z <- search()
  #if(sum(z == "package:nleqslv") == 0) library(nleqslv,quietly=TRUE)

#f=formula
#d=subdata
translate <- function(f,d) {
  
  reg <- glm(f, family="binomial", d)
  
  datalin <- d
  nom.vdep <- all.vars(as.formula(f))[1]
  if(is.numeric(d[,nom.vdep])==FALSE) {
    levels(datalin[,nom.vdep]) <- c('0','1')
    datalin[,nom.vdep] <- as.numeric(as.character(datalin[,nom.vdep]))
    }
  reglin <- lm(f,datalin)

  pur <- function(x) {
    y <- numeric(nl)
    y[1] <- sum(m*x)/sum(m) - p
    for(j in 2:nl) y[j] <- (x[j]/(1-x[j])) / (x[1]/(1-x[1])) - c[j-1]
    y
  }
  nvarind <- length(reg$xlevels)
  pure <- list()
  length(pure) <- nvarind
  nlev <- lapply(reg$xlevels,length)
  icoef <- as.list(c(2,(2+cumsum(unlist(nlev)-1)))[1:nvarind])
  names(icoef) <- names(nlev)
  p <- prop.table(table(reg$model[,1]))[2]
  for(i in 1:nvarind) {
    m <- table(reg$model[,i+1])
    nl <- nlev[[i]]
    c <- exp(reg$coefficients[icoef[[i]]:(icoef[[i]]+nl-2)])
    xstart <- prop.table(table(reg$model[,i+1],reg$model[,1]),1)[,2]
    xstart[xstart==0] <- xstart[xstart==0]+0.0000001 ## NEW 20-02-2017
    result <- nleqslv(xstart, pur, control=list(btol=.01))$x
    names(result) <- reg$xlevels[[i]]
    pure[[i]] <- result
  }

  coef <- numeric()
  for(i in 1:nvarind) {
    co <- numeric(length=nlev[[i]])
    co[1] <- 0
    co[2:nlev[[i]]] <- reg$coefficients[icoef[[i]]:(icoef[[i]]+nlev[[i]]-2)]
    coef <- c(coef,co)
  }
  coef <- c(reg$coefficients[1],coef)
  
  coeflin <- numeric()
  for(i in 1:nvarind) {
    co <- numeric(length=nlev[[i]])
    co[1] <- 0
    co[2:nlev[[i]]] <- reglin$coefficients[icoef[[i]]:(icoef[[i]]+nlev[[i]]-2)]
    coeflin <- c(coeflin,co)
  }
  coeflin <- c(reglin$coefficients[1],coeflin)
  
  disj <- cbind(intercept=rep(1,times=nrow(reg$model)),dichotom(reg$model[,-1],out='numeric'))
  icol <- as.list(c(2,(2+cumsum(unlist(nlev))))[1:nvarind])
  names(icol) <- names(nlev)
  un <- rep(1,times=nrow(reg$model))
  zero <- rep(0,times=nrow(reg$model))
  expe <- list()
  length(expe) <- nvarind
  expe.lin <- list()
  length(expe.lin) <- nvarind
  for(i in 1:nvarind) {
    result <- numeric(length=nlev[[i]])
    resultlin <- numeric(length=nlev[[i]])
    for(j in 1:nlev[[i]]) {
      T <- as.matrix(disj)
      for(k in 1:nlev[[i]]) T[,icol[[i]]+k-1] <- ifelse(j==k,un,zero)
      result[j] <- mean(1/(1+exp(-T%*%coef)))
      resultlin[j] <- mean(T%*%coeflin)
      }
    names(result) <- reg$xlevels[[i]]
    names(resultlin) <- reg$xlevels[[i]]
    expe[[i]] <- result
    expe.lin[[i]] <- resultlin
    }

  raw <- numeric()
  for(i in 1:nvarind) {
    z <- table(reg$model[,i+1],reg$model[,1]) / rowSums(table(reg$model[,i+1],reg$model[,1]))
    raw <- c(raw,as.data.frame.matrix(z)[,2])
    }

  percents <- data.frame(raw,expe=unlist(expe),pure=unlist(pure),expe.lin=unlist(expe.lin))
  rownames(percents) <- colnames(dichotom(reg$model))[-c(1:2)]
  
  final <- list(glm=reg,summary=summary(reg),percents=round(percents,6))
  return(final)
}

translate.binom <- function(ff,dd,Nit) {
  res <- translate(ff,dd)
  if(Nit==0) boot=NULL
  if(Nit>0) {
    #nval <- nrow(res$percents)*ncol(res$percents)
    nval <- prod(dim(res$percents))
    Z1 <- unlist(lapply(1:Nit,function(x) as.numeric(as.matrix(translate(ff,dd[sample(1:nrow(dd),replace=TRUE),])$percents))))
    #rm(.GlobalEnv$.Random.seed) #, envir=globalenv())
    Z2 <- lapply(1:nval,function(x) Z1[seq.int(from=x,to=nval*(Nit-1)+x,by=nval)])
    Z3 <- lapply(Z2,function(x) paste('[',paste(round(quantile(x, c(0.025, 0.975), na.rm=TRUE),4),collapse=';'),']',sep=''))
    boot <- data.frame(matrix(unlist(Z3),nrow=nrow(res$percents),ncol=ncol(res$percents)))
    dimnames(boot) <- dimnames(res$percents)
    }
  res$boot.ci <- boot 
  return(res)
  }

nom.vdep <- all.vars(as.formula(formula))[1]
if(is.factor(data[,nom.vdep])==FALSE) data[,nom.vdep] <- factor(data[,nom.vdep])
vdep <- data[,nom.vdep]
if(nlevels(vdep)==2) return(translate.binom(formula,data,nit))
if(nlevels(vdep)>=3) {
  #z <- search()
  #if(sum(z == "package:nnet") == 0) library(nnet,quietly=TRUE)
  nom.vind <- all.vars(as.formula(formula))[-1]
  ncat <- sum(unlist(lapply(nom.vind,function(x) nlevels(data[,x]))))
  rowtot <- unlist(apply(as.data.frame(data[,nom.vind]),2,function(x) as.numeric(table(x)))) #new
  
  translate2 <- function(fff,ddd) {
    raw <- matrix(0,nrow=ncat,ncol=nlevels(vdep))
    expe <- matrix(0,nrow=ncat,ncol=nlevels(vdep))
    pure <- matrix(0,nrow=ncat,ncol=nlevels(vdep))
    expe.lin <- matrix(0,nrow=ncat,ncol=nlevels(vdep))
    for(i in 2:nlevels(ddd[,nom.vdep])){
      subdata <- ddd[ddd[,nom.vdep] %in% levels(ddd[,nom.vdep])[c(1,i)],]
      subdata[,nom.vdep] <- factor(subdata[,nom.vdep])	
      subrowtot <- unlist(apply(as.data.frame(subdata[,nom.vind]),2,function(x) as.numeric(table(x))))  #new
      binom <- translate.binom(fff,subdata,0)
      raw[,i] <- binom$percents[,'raw']*subrowtot/rowtot
      expe[,i] <- binom$percents[,'expe']*subrowtot/rowtot
      pure[,i] <- binom$percents[,'pure']*subrowtot/rowtot
      expe.lin[,i] <- binom$percents[,'expe.lin']*subrowtot/rowtot
      }
    raw[,1] <- 1-rowSums(raw)
    expe[,1] <- 1-rowSums(expe)
    pure[,1] <- 1-rowSums(pure)
    expe.lin[,1] <- 1-rowSums(expe.lin)
    dimnames(raw) <- list(colnames(dichotom(ddd[,nom.vind])),levels(vdep))
    dimnames(expe) <- list(colnames(dichotom(ddd[,nom.vind])),levels(vdep))
    dimnames(pure) <- list(colnames(dichotom(ddd[,nom.vind])),levels(vdep))
    dimnames(expe.lin) <- list(colnames(dichotom(ddd[,nom.vind])),levels(vdep))
    pct <- list(raw=raw,expe=expe,pure=pure,expe.lin=expe.lin)
    return(pct)
    }
  
  reg <- multinom(formula,data,model=TRUE)
  res <- list(glm=reg,summary=summary(reg),percents=translate2(formula,data))
  
  if(nit==0) mboot <- NULL
  if(nit>0) {
    mboot <- vector("list", 4)
    W1 <- lapply(1:nit,function(x) translate2(formula,data[sample(1:nrow(data),replace=TRUE),]))
    #rm(.GlobalEnv$.Random.seed) #, envir=globalenv())
    nval <- prod(dim(W1[[1]][[1]]))
    for(i in 1:4) {
      W2 <- unlist(lapply(W1,function(x) x[[i]]))
      W3 <- lapply(1:nval,function(x) W2[seq(from=x,to=nval*(nit-1)+x,by=nval)])
      W4 <- lapply(W3,function(x) paste('[',paste(round(quantile(x, c(0.025, 0.975), na.rm=TRUE),4),collapse=';'),']',sep=''))  #new
      mboot[[i]] <- data.frame(matrix(unlist(W4),nrow=nrow(W1[[1]][[1]]),ncol=ncol(W1[[1]][[1]])))
      dimnames(mboot[[i]]) <- dimnames(W1[[1]][[1]])
      }
    names(mboot) <- c('raw','expe','pure','expe.lin') 
    res$boot.ci <- mboot
    }
    
  return(res)
  }
}

