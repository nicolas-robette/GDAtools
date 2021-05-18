\name{ggadd_supind}

\alias{ggadd_supind}

\title{Adds supplementary individuals to a cloud of individuals}

\description{Adds supplementary individuals to a MCA cloud of the individuals, using the ggplot2 framework.}

\usage{ggadd_supind(p, resmca, dfsup, axes=c(1,2), col="black", textsize=5, pointsize=2)
}

\arguments{
  \item{p}{ggplot object with the cloud of variables}
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}.}
  \item{dfsup}{data frame with the supplementary individuals. It must have the same factors as the data frame used as input for the initial MCA.}
  \item{axes}{numeric vector of length 2, specifying the dimensions (axes) to plot (default is c(1,2))}
  \item{col}{color for the labels and points of the individuals (default is black)}
  \item{textsize}{Size of the labels of the individuals. Default is 5.}
  \item{pointsize}{Size of the points of the individuals. If NULL, only labels are plotted. Default is 2.}
}

\details{
The function uses the row names of \code{dfsup} as labels for the individuals.
}

\author{Nicolas Robette}

\seealso{
\code{\link{indsup}}, \code{\link{ggcloud_indiv}}
}

\examples{
## Performs a specific MCA on 'Music' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## plots the cloud of individuals,
## and then adds individuals 1, 20 and 300 as supplementary individuals.
data(Music)
rownames(Music) <- paste0("i", 1:nrow(Music))
mca <- speMCA(Music[,1:5], excl=c(3,6,9,12,15))
p <- ggcloud_indiv(mca, col="lightgrey")
ggadd_supind(p, mca, Music[c(1,20,300),1:5])
}

\keyword{ aplot }
\keyword{ multivariate }
