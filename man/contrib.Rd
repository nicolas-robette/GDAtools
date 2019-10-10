\name{contrib}

\alias{contrib}

\title{Computes contributions for a correspondence analysis}

\description{From MCA results, computes contributions of categories and variables to the axes and the overall cloud.}

\usage{contrib(resmca)}

\arguments{
  \item{resmca}{object of class \code{'MCA'}, \code{'speMCA'} or \code{'csMCA'}}
}

\details{
The contribution of a point to an axis depends both on the distance from the point to the origin point along the axis 
and on the weight of the point. The contributions of points to axes are the main aid to interpretation 
(see Le Roux and Rouanet, 2004 and 2010).
}

\value{
  A list of data frames:
  \item{ctr}{Data frame with the contributions of categories to axes}
  \item{var.ctr}{Data frame with the contributions of variables to axes}
  \item{ctr.cloud}{Data frame with the contributions of categories to the overall cloud}
  \item{vctr.cloud}{Data frame with the contributions of variables to the overall cloud}
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{MCA}}, \code{\link{speMCA}}, \code{\link{csMCA}}, \code{\link{varsup}}
}

\examples{
## Performs a specific MCA on the 'Music' example data set
## and compute contributions
data(Music)
mca <- speMCA(Music[,1:5],excl=c(3,6,9,12,15))
contrib(mca)
}

\keyword{ multivariate }
\keyword{ misc }
