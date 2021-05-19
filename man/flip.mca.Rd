\name{flip.mca}

\alias{flip.mca}

\title{Flips the coordinates of a MCA}

\description{Flips the coordinates of the individuals and the categories on one or more dimensions of a MCA.}

\usage{flip.mca(resmca, dim=1)}

\arguments{
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}}
  \item{dim}{the dimensions for which the coordinates are flipped. By default, only the 1st dimension is flipped}
}

\value{
  Returns an object of the same class as \code{resmca}
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{MCA}}, \code{\link{speMCA}}, \code{\link{csMCA}}
}

\examples{
## Flips dimensions 1 and 2
## of the MCA of 'Music' example data set
data(Music)
mca <- speMCA(Music[,1:5])
ggcloud_variables(mca, legend="none")
flipped_mca <- flip.mca(mca, dim=c(1,2))
ggcloud_variables(flipped_mca, legend="none")
}

\keyword{ multivariate }
\keyword{ misc }
