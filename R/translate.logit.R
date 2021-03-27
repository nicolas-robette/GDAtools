translate.logit <- function(formula,data,nit=0) {

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
  res <- list()
  res[[1]] <- 1
  for(i in 2:nlevels(data[,nom.vdep])) {
    newdt <- data[data[,nom.vdep] %in% levels(data[,nom.vdep])[c(1,i)],]
    newdt[,nom.vdep] <- factor(newdt[,nom.vdep])
    res[[i]] <- translate.binom(formula, newdt, Nit=0)$percents
    res[[1]] <- res[[1]] - res[[i]]
  }
  names(res) <- levels(data[,nom.vdep])
  res2 <- list()
  res2[[1]] <- do.call("cbind.data.frame",lapply(res, function(x) x$raw))
  res2[[2]] <- do.call("cbind.data.frame",lapply(res, function(x) x$expe))
  res2[[3]] <- do.call("cbind.data.frame",lapply(res, function(x) x$pure))
  res2[[4]] <- do.call("cbind.data.frame",lapply(res, function(x) x$expe.lin))
  names(res2) <- c("raw","expe","pure","expe.lin")
  dimnames(res2[["raw"]]) <- dimnames(res2[["expe"]]) <- dimnames(res2[["pure"]]) <- dimnames(res2[["expe.lin"]]) <- list(rownames(res[[1]]), names(res))
  resfin <- list("by_category"=res, "by_method"=res2)
  reg <- multinom(formula,data,model=TRUE)
  res <- list(glm=reg, summary=summary(reg),percents=resfin)
  return(res)
  }
}

