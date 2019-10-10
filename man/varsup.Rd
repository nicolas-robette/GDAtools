\name{varsup}

\alias{varsup}

\title{Computes statistics for a categorical supplementary variable}

\description{
   From MCA results, computes statistics (weights, coordinates, contributions, test-values, variances) for 
   a categorical supplementary variable.
}

\usage{varsup(resmca, var)}

\arguments{
  \item{resmca}{object of class \code{'MCA'}, \code{'speMCA'}, \code{'csMCA'}, \code{'stMCA'} or \code{'multiMCA'}}
  \item{var}{the categorical supplementary variable. It does not need to have been used at the MCA step.}
}

\value{
  Returns a list:
  \item{weight}{numeric vector of categories weights}
  \item{coord}{data frame of categories coordinates}
  \item{cos2}{data frame of categories square cosine}
  \item{var}{data frame of categories within variances, variance between and within categories and variable square correlation ratio (eta2)}
  \item{v.test}{data frame of categories test-values}
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{speMCA}}, \code{\link{csMCA}}, \code{\link{multiMCA}}, \code{\link{textvarsup}}
}

\examples{
## Performs a specific MCA on 'Music' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## and then computes statistics for age supplementary variable.
data(Music)
getindexcat(Music)
mca <- speMCA(Music[,1:5],excl=c(3,6,9,12,15))
varsup(mca,Music$Age)
}

\keyword{ multivariate }
\keyword{ misc }
