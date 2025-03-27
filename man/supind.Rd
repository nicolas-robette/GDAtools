\name{supind}

\alias{supind}
\alias{indsup}

\title{Statistics for supplementary individuals}

\description{
   From MCA results, computes statistics (coordinates, squared cosines) for 
   supplementary individuals.
}

\usage{
supind(resmca, supdata)

indsup(resmca, supdata)
}

\arguments{
  \item{resmca}{object created with \code{MCA}, \code{speMCA}, \code{csMCA}, \code{wcMCA} or \code{bcMCA} function}
  \item{supdata}{data frame with the supplementary individuals. It must have the same factors as the data frame used as input for the initial MCA.}
}

\value{
  Returns a list with the following items :
  \item{coord}{matrix of individuals coordinates}
  \item{cos2}{matrix of individuals squared cosines}
}

\note{
\code{indsup} is softly deprecated. Please use \code{supind} instead.
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{ggadd_supind}},\code{\link{textindsup}}, \code{\link{supvar}}, \code{\link{supvars}}
}

\examples{
# specific MCA of Music example data set
# excluding the first two observations
data(Music)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA")
mca <- speMCA(Music[3:nrow(Music),1:5], excl = junk)
# computes coordinates and squared cosines
# of the first two (supplementary) observations
supind(mca,Music[1:2,1:5])
}

\keyword{ multivariate }
\keyword{ misc }
