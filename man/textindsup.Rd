\name{textindsup}

\alias{textindsup}

\title{Plot of supplementary individuals}

\description{Adds supplementary individuals to a MCA cloud of the individuals.}

\usage{textindsup(resmca, supdata, axes = c(1, 2), col = "darkred")
}

\arguments{
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, or \code{csMCA}}
  \item{supdata}{data frame with the supplementary individuals. It must have the same factors as the data frame used as input for the initial MCA.}
  \item{axes}{numeric vector of length 2, specifying the dimensions (axes) to plot (default is c(1,2))}
  \item{col}{color for the labels of the categories (default is "darkred")}
}

\author{Nicolas Robette}

\seealso{
\code{\link{supind}}, \code{\link{plot.speMCA}}, \code{\link{plot.csMCA}}
}

\examples{
# specific MCA of Music example data set
# excluding the first two observations
data(Music)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA")
mca <- speMCA(Music[3:nrow(Music), 1:5], excl = junk)
# cloud of active individuals
# with the two supplementary individuals
plot(mca, type = "i")
textindsup(mca, Music[1:2, 1:5])
}

\keyword{ aplot }
\keyword{ multivariate }
