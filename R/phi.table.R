phi.table <- function(x,y,dec=3) {
  tab <- cor(dichotom(factor(x),out='numeric'),
             dichotom(factor(y),out='numeric'),
             method='pearson',
             use='complete.obs')
  tab <- as.table(round(tab,dec))
  rownames(tab) <- levels(factor(x))
  colnames(tab) <- levels(factor(y))
  tab
}
