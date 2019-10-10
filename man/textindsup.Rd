\name{textindsup}

\alias{textindsup}

\title{Adds supplementary individuals to a MCA graph}

\description{Adds supplementary individuals to a MCA graph of the cloud of the individuals.}

\usage{textindsup(resmca, supdata, axes = c(1, 2), col = "darkred")
}

\arguments{
  \item{resmca}{object of class \code{'MCA'}, \code{'speMCA'}, or \code{'csMCA'}}
  \item{supdata}{data frame with the supplementary individuals. It must have the same factors as the data frame used
                 as input for the initial MCA.}
  \item{axes}{numeric vector of length 2, specifying the dimensions (axes) to plot (default is c(1,2))}
  \item{col}{color for the labels of the categories (default is 'darkred')}
}

\author{Nicolas Robette}

\seealso{
\code{\link{indsup}}, \code{\link{plot.speMCA}}, \code{\link{plot.csMCA}}
}

\examples{
## Performs a specific MCA on 'Music' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## plots the cloud of individuals,
## and then adds supplementary individuals.
data(Music)
getindexcat(Music)
mca <- speMCA(Music[3:nrow(Music),1:5],excl=c(3,6,9,12,15))
plot(mca,type='i')
textindsup(mca,Music[1:2,1:5])
}

\keyword{ aplot }
\keyword{ multivariate }
