\name{homog.test}

\alias{homog.test}

\title{Computes a homogeneity test for a categorical supplementary variable}

\description{
   From MCA results, computes a homogeneity test for a categorical supplementary variable, i.e. 
   characterizes the homogeneity of several subclouds.
}

\usage{homog.test(resmca, var)}

\arguments{
  \item{resmca}{object of class \code{'MCA'}, \code{'speMCA'}, \code{'csMCA'}, \code{'stMCA'} or \code{'multiMCA'}}
  \item{var}{the categorical supplementary variable. It does not need to have been used at the MCA step.}
}

\value{
  Returns a list of square matrices, one per MCA dimension. Each matrix gives the test statistic for any pair of categories.
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{speMCA}}, \code{\link{csMCA}}, \code{\link{stMCA}}, \code{\link{multiMCA}}, \code{\link{textvarsup}}
}

\examples{
## Performs a specific MCA on 'Music' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## and then computes a homogeneity test for age supplementary variable.
data(Music)
getindexcat(Music)
mca <- speMCA(Music[,1:5],excl=c(3,6,9,12,15))
homog.test(mca,Music$Age)
}

\keyword{ multivariate }
\keyword{ misc }
