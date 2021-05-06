\name{ggadd_attractions}
\alias{ggadd_attractions}

\title{Adds attractions between categories via segments to a cloud of variables}

\description{
Adds attractions between categories, as measured by phi coefficients or percentages of maximum deviation (PEM), by plotting segments onto a MCA cloud of variables, using the ggplot2 framework.
}

\usage{
ggadd_attractions(p, resmca, axes=c(1,2), measure="phi", min.asso=0.3,
col.segment="lightgray", col.text="black", text.size=3)
}

\arguments{
  \item{p}{ggplot object with the cloud of variables}
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}.}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{measure}{character. The measure for attractions: "phi" (default) for phi coefficients, "pem" for percentages of maximum deviation (PEM).}
  \item{min.asso}{numerical value between 0 and 1. The minimal attraction value for segments to be plotted. Default is 0.3.}
  \item{col.segment}{Character. Color of the segments. Default is lightgray.}
  \item{col.text}{Character. Color of the labels of the categories. Default is black.}
  \item{text.size}{Size of the labels of categories. Default is 3.}
}

\value{a ggplot object}

\references{
Cibois, Philippe. \emph{Les méthodes d’analyse d’enquêtes}. Nouvelle édition [en ligne]. Lyon: ENS Éditions, 2014. Disponible sur Internet : <http://books.openedition.org/enseditions/1443>
}

\author{Nicolas Robette}

\seealso{
\code{\link{ggcloud_variables}}, \code{\link{ggcloud_indiv}}, \code{\link{ggadd_ellipses}}, \code{\link{ggadd_corr}}, \code{\link{ggadd_interaction}}, \code{\link{ggadd_density}}
}

\examples{
## Performs a specific MCA on 'Taste' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## draws the cloud of categories
## and adds segments between categories with attractions.
data(Taste)
getindexcat(Taste[,1:11])
mca <- speMCA(Taste[,1:11],excl=c(3,6,9,12,15,18,21,24,27,30,33))
p <- ggcloud_variables(mca, col="white", legend="none")
ggadd_attractions(p, mca, measure="phi", min.asso=0.1)
}

\keyword{ aplot }
\keyword{ multivariate }
