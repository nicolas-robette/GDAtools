\name{ahc_plots}
\alias{ahc_plots}

\title{Plots for AHC}

\description{
Draws various plots for Ascending Hierarchical Clustering results.
}

\usage{
ahc_plots(ahc, distance = NULL, max.cl = 20, type = "dist")
}

\arguments{
  \item{ahc}{object of class \code{hclust} or \code{agnes}}
  \item{distance}{A dissimilarity matrix or a \code{dist} object. Only used if type is "inert" or "loss". Default is NULL.}
  \item{max.cl}{Integer. Maximum number of clusters taken into account in the plots.}
  \item{type}{Character string. If "dist" (default), the distance between agregated clustersis plotted. If "inert", it is the percentage of explained inertia (pseudo-R2). If "loss", it is the relative loss of explained inertia (pseudo-R2).}
}

\author{Nicolas Robette}

\examples{
data(Taste)
disjonctif <- dichotom(Taste[,1:11])
distance <- dist(disjonctif)
cah <- stats::hclust(distance, method = "ward.D2")
ahc_plots(cah, max.cl = 15, type = "dist")
ahc_plots(cah, distance = distance, max.cl = 15, type = "inert")
ahc_plots(cah, distance = distance, max.cl = 15, type = "loss")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ misc }
