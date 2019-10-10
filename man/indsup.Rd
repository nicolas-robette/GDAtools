\name{indsup}

\alias{indsup}

\title{Computes statistics for supplementary individuals}

\description{
   From MCA results, computes statistics (coordinates, squared cosines) for 
   supplementary individuals.
}

\usage{indsup(resmca, supdata)}

\arguments{
  \item{resmca}{object of class \code{'MCA'}, \code{'speMCA'} or \code{'csMCA'}}
  \item{supdata}{data frame with the supplementary individuals. It must have the same factors as the data frame used
                 as input for the initial MCA.}
}

\value{
  Returns a list:
  \item{coord}{matrix of individuals' coordinates}
  \item{cos2}{matrix of individuals' square cosines}
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{textindsup}}, \code{\link{speMCA}}, \code{\link{csMCA}}, \code{\link{varsup}}
}

\examples{
## Performs a specific MCA on 'Music' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## and then computes statistics for supplementary individuals.
data(Music)
getindexcat(Music)
mca <- speMCA(Music[3:nrow(Music),1:5],excl=c(3,6,9,12,15))
indsup(mca,Music[1:2,1:5])
}

\keyword{ multivariate }
\keyword{ misc }
