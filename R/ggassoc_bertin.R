ggassoc_bertin <- function(data, mapping, prop.width = FALSE, sort = "none", ncol = 2, add.rprop = FALSE) {
  
  xVal <- rlang::eval_tidy(mapping$x, data)
  yVal <- rlang::eval_tidy(mapping$y, data)
  xName <- rlang::as_name(mapping$x)
  yName <- rlang::as_name(mapping$y)
  
  if(sort!="none") {
    temp <- MASS::corresp(~xVal+yVal,nf=1)
    if(sort %in% c("x","both")) xVal <- factor(xVal, levels=names(sort(temp$rscore)))
    if(sort %in% c("y","both")) yVal <- factor(yVal, levels=names(sort(temp$cscore)))
  }
  
  res0 <- assoc.twocat(xVal, yVal)
  res <- res0$gather
  
  res1 <- res
  res1$co <- rep("A", nrow(res))
  res1$h <- ifelse(res1$rprop > res1$prop.y, 0, res1$rprop)
  
  res2 <- res
  res2$co <- rep("B", nrow(res))
  res2$h <- ifelse(res2$rprop > res2$prop.y, res2$prop.y, 0)
  
  res3 <- res
  res3$co <- rep("C", nrow(res))
  res3$h <- ifelse(res3$rprop > res3$prop.y, res3$rprop-res3$prop.y, 0)
  
  restot <- rbind(res1,res2,res3)
  restot$co <- factor(restot$co, levels=c("C","B","A"))
  
  if(prop.width) { 
    restot$wi <- restot$prop.x
  } else {
    restot$wi <- rep(1,nrow(restot))
  }
  
  if(ncol==3) {
    cols <- c("black","gray","white")
  } else {
    cols <- c("black","black","white")
  }

  p <- ggplot2::ggplot(restot, ggplot2::aes(x = 1, y = .data$h, fill = .data$co, width = .data$wi)) +
    ggplot2::geom_col(col = "black") +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::facet_grid(var.y ~ var.x, scales = "free_x", space = "free") +
    ggplot2::xlab(xName) +
    ggplot2::ylab(yName) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   strip.text.y.right = ggplot2::element_text(angle = 0, hjust = 0))
  
  if(add.rprop) p <- p +
    ggplot2::geom_text(data = restot[restot$co=="A",], 
                       ggplot2::aes(y = .data$rprop, label = round(100*.data$rprop,1)),
                       size = ggplot2::rel(2), vjust = -0.5)
  p
}

# ggassoc_bertin(Movies, ggplot2::aes(x = Country, y = Genre), sort = "both", prop.width = TRUE)
# ggassoc_bertin(Movies, ggplot2::aes(x = Country, y = Genre), sort = "both", prop.width = TRUE, add.rprop = TRUE)
# ggassoc_bertin(Movies, ggplot2::aes(x = Country, y = Genre), sort = "both", prop.width = TRUE, ncol = 3)
# ggassoc_bertin(Movies, ggplot2::aes(x = Country, y = Genre), sort = "both", prop.width = TRUE, ncol = 3, add.rprop = TRUE)

