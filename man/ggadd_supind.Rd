\name{ggadd_supind}

\alias{ggadd_supind}

\title{Plot of supplementary individuals}

\description{Adds supplementary individuals to a MCA cloud of the individuals}

\usage{ggadd_supind(p, resmca, dfsup, axes = c(1,2), 
col = "black", textsize = 5, pointsize = 2)
}

\arguments{
  \item{p}{\code{ggplot2} object with the cloud of individuals.}
  \item{resmca}{object created with \code{MCA}, \code{speMCA}, \code{csMCA}, \code{wcMCA}, \code{bcMCA}, \code{stMCA} or \code{multiMCA} function}
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
\code{\link{supind}}, \code{\link{ggcloud_indiv}}
}

\examples{
# specific MCA of Music example data set
data(Music)
rownames(Music) <- paste0("i", 1:nrow(Music))
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA")
mca <- speMCA(Music[,1:5], excl = junk)
# adds individuals 1, 20 and 300 as supplementary individuals 
# onto the cloud of individuals
p <- ggcloud_indiv(mca, col = "lightgrey")
ggadd_supind(p, mca, Music[c(1,20,300), 1:5])
}

\keyword{ aplot }
\keyword{ multivariate }
