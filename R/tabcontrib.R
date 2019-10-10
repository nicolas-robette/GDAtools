tabcontrib <- function(resmca,dim=1) {
	z <- dimcontrib(resmca,dim)$var[[1]]
	z$var <- unlist(lapply(strsplit(rownames(z),'.',fixed=TRUE),function(x) x[1]))
	z$moda <- unlist(lapply(strsplit(rownames(z),'.',fixed=TRUE),function(x) paste(x[-1],collapse='.')))
	w <- aggregate(abs(z$ctr),by=list(z$var),sum)
	w <- w[order(w$x,decreasing=TRUE),]
	names(w) <- c('var','ctrtot')
	w$rank <- 1:nrow(w)
	wz <- merge(z,w,by='var',all.x=TRUE,all.y=FALSE)
	wz <- wz[order(wz$rank,wz$ctr),]
	wz$count <- unlist(lapply(rle(as.vector(wz$var))$lengths,function(x){1:x}))
	wz$ctr2 <- wz$ctr1 <- wz$ctr
	wz$ctr1[wz$ctr1>0] <- ""
	wz$ctr2[wz$ctr2<0] <- ""
	wz$var[wz$count>1] <- ""
	wz$ctrtot[wz$count>1] <- ""
	wz$cumctrtot <- character(length=nrow(wz))
	wz$cumctrtot[!wz$ctrtot==""] <- cumsum(as.numeric(wz$ctrtot[!wz$ctrtot==""]))
	wz <- wz[,c('var','moda','ctr1','ctr2','weight','ctrtot','cumctrtot')]
	return(wz)
	}