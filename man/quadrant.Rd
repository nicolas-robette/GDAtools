\name{quadrant}

\alias{quadrant}

\title{Quadrant of active individuals}

\description{
Computes the quadrant of active individuals from a MCA.
}

\usage{
quadrant(resmca, dim = c(1,2))
}

\arguments{
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, or \code{csMCA}}
  \item{dim}{dimensions of the space (default is c(1,2))}
}

\value{
  Returns a factor with four levels : upper_left, lower_left, upper_right, lower_right
}

\author{Nicolas Robette}

\seealso{
\code{\link{speMCA}}, \code{\link{csMCA}}
}

\examples{
# specific MCA of Music example data set
data(Music)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA")
mca <- speMCA(Music[,1:5], excl = junk)
# distribution of the quadrants
table(quadrant(mca, c(1,2)))
}

\keyword{ multivariate }
\keyword{ misc }
