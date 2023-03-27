\name{ggadd_attractions}
\alias{ggadd_attractions}

\title{Plot of attractions between categories}

\description{
Adds attractions between categories, as measured by phi coefficients or percentages of maximum deviation (PEM), by plotting segments onto a MCA cloud of variables.
}

\usage{
ggadd_attractions(p, resmca, axes = c(1,2), measure = "phi", min.asso = 0.3,
col.segment = "lightgray", col.text = "black", text.size = 3)
}

\arguments{
  \item{p}{\code{ggplot2} object with the cloud of variables}
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}.}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{measure}{character string. The measure for attractions: "phi" (default) for phi coefficients, "pem" for percentages of maximum deviation (PEM).}
  \item{min.asso}{numerical value ranging from 0 to 1. The minimal attraction value for segments to be plotted. Default is 0.3.}
  \item{col.segment}{Character string with the color of the segments. Default is lightgray.}
  \item{col.text}{Character string with the color of the labels of the categories. Default is black.}
  \item{text.size}{Size of the labels of categories. Default is 3.}
}

\value{a \code{ggplot2} object}

\references{
Cibois, Philippe. \emph{Les méthodes d’analyse d’enquêtes}. Nouvelle édition [en ligne]. Lyon: ENS Éditions, 2014. <http://books.openedition.org/enseditions/1443>
}

\author{Nicolas Robette}

\seealso{
\code{\link{ggcloud_variables}}
}

\examples{
# specific MCA on Taste example data set
data(Taste)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA",
          "Comedy.NA", "Crime.NA", "Animation.NA", "SciFi.NA", "Love.NA", 
          "Musical.NA")
mca <- speMCA(Taste[,1:11], excl = junk)
# Plots attractions
p <- ggcloud_variables(mca, col="white", legend="none")
ggadd_attractions(p, mca, measure="phi", min.asso=0.1)
}

\keyword{ aplot }
\keyword{ multivariate }
