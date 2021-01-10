phi.table <- function(x,y,weights=rep(1,length(x)),digits=3) {
  # tab <- cor(dichotom(factor(x),out='numeric'),
  #            dichotom(factor(y),out='numeric'),
  #            method='pearson',
  #            use='complete.obs')
  tab <- wdm::wdm(GDAtools::dichotom(factor(x),out='numeric'),
                  GDAtools::dichotom(factor(y),out='numeric'),
                  method = "pearson",
                  weights = weights,
                  remove_missing = TRUE)
  tab <- as.table(tab)
  rownames(tab) <- levels(factor(x))
  colnames(tab) <- levels(factor(y))
  if(!is.null(digits)) tab <- round(tab,digits)
  return(tab)
}
