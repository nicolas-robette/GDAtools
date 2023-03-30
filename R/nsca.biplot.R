nsca.biplot <- function(nsca, axes = c(1,2)) {
  
  rate1 <- round(nsca$eig[axes[1], 2], 1)
  rate2 <- round(nsca$eig[axes[2], 2], 1)
  
  co1 <- data.frame(nsca$row$coord)
  co1$rc <- rep("row", nrow(co1))
  co1$size = rep(1, nrow(co1))
  co2 <- data.frame(sweep(nsca$col$coord, 2, sqrt(nsca$eig[,1]), "/"))
  co2$rc <- rep("col", nrow(co2))
  co2$size = rep(2, nrow(co2))
  co <- rbind.data.frame(co1, co2)
  co$intercept <- rep(0, nrow(co))
  co$slope <- co[,axes[2]] / co[,axes[1]]
  co$x <- co[,axes[1]]
  co$y <- co[,axes[2]]
  co$names <- rownames(co)
  
  ggplot2::ggplot(data = co) +
    ggplot2::geom_abline(data = co[co$rc=="row",], ggplot2::aes(intercept = 0, slope = .data$slope),
                         linewidth = .3, linetype = "dashed", color = "darkgrey", alpha = .5) +
    ggplot2::geom_segment(data = co[co$rc=="row",], ggplot2::aes(x = 0, y = 0, xend = .data$x, yend = .data$y),
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),
                          color = "darkgrey", alpha = .5, linewidth = .4) +
    ggplot2::geom_hline(yintercept = 0, colour = "darkgrey", linewidth = .1) + 
    ggplot2::geom_vline(xintercept = 0, colour = "darkgrey", linewidth = .1) + 
    ggplot2::geom_text(data = co, ggplot2::aes(x = .data$x, y = .data$y, label = .data$names, color = .data$rc, size = .data$size)) +
    ggplot2::scale_size_continuous(range = c(4,6)) +
    ggplot2::xlab(paste0("dim ", axes[1], " (", rate1, " %)")) +
    ggplot2::ylab(paste0("dim ", axes[2], " (", rate2, " %)")) +
    ggplot2::theme_bw() + 
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "none")
}