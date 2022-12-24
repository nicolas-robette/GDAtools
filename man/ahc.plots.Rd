\name{ahc.plots}
\alias{ahc.plots}

\title{Plots for AHC}

\description{
Draws various plots for Ascending Hierarchical Clustering results.
}

\usage{
ahc.plots(ahc, distance = NULL, max.cl = 20, type = "dist")
}

\arguments{
  \item{ahc}{object of class \code{hclust} or \code{agnes}}
  \item{distance}{A dissimilarity matrix or a \code{dist} object. Only used if type is "inert" or "loss". Default is NULL.}
  \item{max.cl}{Integer. Maximum number of clusters taken into account in the plots.}
  \item{type}{Character string. If "dist" (default), the distance between agregated clusters is plotted. If "inert", it is the percentage of explained inertia (pseudo-R2). If "loss", it is the relative loss of explained inertia (pseudo-R2).}
}

\details{
The three kinds of plots proposed with this function are aimed at guiding in the choice of the number of clusters.
}

\author{Nicolas Robette}

\examples{
data(Taste)
disjonctif <- dichotom(Taste[,1:11])
distance <- dist(disjonctif)
cah <- stats::hclust(distance, method = "ward.D2")
ahc.plots(cah, max.cl = 15, type = "dist")
ahc.plots(cah, distance = distance, max.cl = 15, type = "inert")
ahc.plots(cah, distance = distance, max.cl = 15, type = "loss")
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ misc }
