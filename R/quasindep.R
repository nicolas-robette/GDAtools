quasindep <- function(tab, order = 3, tol = 1e-6) {
  if(ncol(tab) != nrow(tab)) stop("tab has to be symmetrical")
  # Reconstitution of order 0
  tab0 <- tab
  while(any(abs(diag(tab0) - rowSums(tab0) * colSums(tab0) / sum(tab0)) > tol)) {
    diag(tab0) <- rowSums(tab0) * colSums(tab0) / sum(tab0)
  }
  ca <- FactoMineR::CA(tab0, graph = FALSE)
  for(i in 1:order) {
    # Reconstitution of order i
    while(any(abs(diag(tab0) - diag(FactoMineR::reconst(ca, ncp = i))) > tol)) {
      # print(mean(abs(diag(tab0) - diag(reconst(ca, ncp = i)))))
      diag(tab0) <- diag(FactoMineR::reconst(ca, ncp = i))
      ca <- FactoMineR::CA(tab0, graph = FALSE, ncp = i+1)
    }
  }
  return(tab0)
}
