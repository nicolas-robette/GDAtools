\name{planecontrib}
\alias{planecontrib}

\title{Contributions to a plane}

\description{
For a given plane of a MCA, computes contributions and squared cosines of the active variables and categories and of the active individuals.
}

\usage{
planecontrib(resmca, axes = c(1,2))
}

\arguments{
  \item{resmca}{object of class \code{MCA}, \code{speMCA} or \code{csMCA}}
  \item{axes}{numeric vector of length 2, specifying the axes forming the plane to describe. Default is c(1,2).}
}

\value{
A list of two lists. The first deals with variables :
\item{ctr12}{vector of contributions of the active categories to the plane}
\item{cos12}{vector of squared cosines of the active categories in the plane}
\item{vctr12}{vector of contributions of the active variables to the plane}
The second deals with observations :
\item{ctr12}{vector of contributions of the observations to the plane}
\item{cos12}{vector of squared cosines of the observations in the plane}
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).}

\author{Nicolas Robette}

\seealso{
\code{\link{dimcontrib}}, \code{\link{tabcontrib}}
}

\examples{
data(Music)
junk <- c("FrenchPop.NA","Rap.NA","Jazz.NA","Classical.NA","Rock.NA")
mca <- speMCA(Music[,1:5], excl = junk)
co <- planecontrib(mca)
co$var
}

\keyword{ multivariate }
\keyword{ misc }
