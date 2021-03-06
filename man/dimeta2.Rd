\name{dimeta2}

\alias{dimeta2}

\title{Describes the eta2 of a list of supplementary variables for the axes of MCA and variants of MCA}

\description{
Computes eta2 for a list of supplementary variables. It allows to analyze variants of MCA, such as 'specific' MCA or 'class specific' MCA.
}

\usage{dimeta2(resmca, l, n, dim = 1:resmca$call$ncp)}

\arguments{
  \item{resmca}{object of class \code{'MCA'}, \code{'speMCA'}, \code{'csMCA'} or \code{'stMCA'}}
  \item{l}{a list of supplementary variables}
  \item{n}{a vector of names for the supplementary variables. The vector's length must be equal to \code{'l'}'s length}
  \item{dim}{the axes for which eta2 are computed. Default is 1:ncp}
}

\value{
  Returns a data frame with supplementary variables as rows and axes as columns.
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{dimdesc}}, \code{\link{dimdesc.MCA}}, \code{\link{dimcontrib}}, \code{\link{condes}}, \code{\link{speMCA}}, \code{\link{csMCA}}
}

\examples{
## Performs a specific MCA on 'Music' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## and then describes the eta2 for Gender and Age (axes 1 and 2).
data(Music)
getindexcat(Music[,1:5])
mca <- speMCA(Music[,1:5],excl=c(3,6,9,12,15))
dimeta2(mca,list(Music$Gender,Music$Age),c('Gender','Age'),dim=1:2)
}

\keyword{ multivariate }
\keyword{ misc }
