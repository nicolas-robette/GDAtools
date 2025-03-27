scaled.dev <- function(resmca, var) {
  sv <- supvar(resmca, var)
  res <- lapply(1:ncol(sv$coord), function(x) round(abs(outer(sv$coord[,x], sv$coord[,x], "-")),3))
  res <- lapply(res, function(x) {rownames(x) <- colnames(x) <- rownames(sv$coord); return(x)})
  names(res) <- colnames(sv$coord)
  return(res)
}
