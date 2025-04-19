barplot_contrib <- function(resmca, dim = 1, 
                            which = "var", sort = FALSE, col = "tomato4", repel = FALSE) {
  
  type <- attr(resmca,'class')[1]
  
  if(type == "bcMCA") {
    if(which=="ind") stop("Contributions of individuals cannot be computed for objects created by bcMCA() function.")
  }
  
  ctr <- resmca[[which]]$contrib * sign(resmca[[which]]$coord)
  ctr <- as.data.frame(ctr)[,dim]
  names(ctr) <- rownames(resmca[[which]]$contrib)
  ctr <- ctr / sum(abs(ctr))

  if(isTRUE(sort)) ctr <- rev(sort(ctr))
  df <- data.frame(Variable = factor(names(ctr), levels = names(ctr)),
                   Weight = ctr,
                   place = ifelse(ctr>=0, 1, 0))
  
  seuil <- 1/length(ctr)
  
  p <-
    ggplot2::ggplot(df, ggplot2::aes(x = .data$Variable, y = .data$Weight)) +
    ggplot2::geom_hline(yintercept = seuil, colour = "gray", linetype = "dashed", alpha = 0.8, linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = 0, colour = "gray", linetype = "solid", alpha = 0.8, linewidth = 0.3) +
    ggplot2::geom_hline(yintercept = -seuil, colour = "gray", linetype = "dashed", alpha = 0.8, linewidth = 0.3) +
    ggplot2::geom_col(color = "white", fill = col, alpha = 0.6) +
    ggplot2::scale_x_discrete(expand = c(0.05, 0.05)) +
    ggplot2::ylab(paste0("Contributions (dim ",dim,")")) +
    ggplot2::xlab("Category") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank())
  
  if(isTRUE(repel)) {
    p <- p + ggrepel::geom_text_repel(ggplot2::aes(label = .data$Variable, y = .data$Weight),
                                      alpha = 0.6)
  } else {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$Variable,
                                             y = .data$Weight+sign(.data$Weight)*0.01,
                                             vjust = .data$place),
                                alpha = 0.6)
  }
  
  p
  
}
