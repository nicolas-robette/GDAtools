\name{ggassoc_bertin}

\alias{ggassoc_bertin}

\title{Bar plot of a crosstabulation inspired by Bertin}

\description{
For a cross-tabulation, plots bars for the conditional percentages of variable y according to variable x, using ggplot2. The general display is inspired by Bertin's plots.
}

\usage{
ggassoc_bertin(data, mapping, prop.width = FALSE, 
sort = "none", ncol = 2, add.rprop = FALSE)
}

\arguments{
  \item{data}{dataset to use for plot}
  \item{mapping}{aesthetic being used. It must specify x and y.}
  \item{prop.width}{logical. If TRUE, the width of the bars is proportional to the margin percentages of variable x.}
  \item{sort}{character. If "both", rows and columns are sorted according to the first factor of a correspondence analysis of the contingency table. If "x", only variable x is sorted. If "y", only variable y is sorted. If "none" (default), no sorting is done.}
  \item{ncol}{Integer. If 2 (default), only white and black are used to fill the bars. If 3, gray is used additionally to fill the part of the bars corresponding to margin percentages of variable y.}
  \item{add.rprop}{logical. If TRUE, row percentages are displayed on top of the bars. Default is FALSE.}
}

\details{
The height of the bars is proportional to the conditional frequency of variable y. The bars are filled in black if the conditional frequency is higher than the marginal frequency; otherwise it's filled in white.
}

\value{
a ggplot object
}

\author{Nicolas Robette}

\references{
J. Bertin: La graphique et le traitement graphique de l'information. Flammarion: Paris 1977.
}

\seealso{
 \code{\link{assoc.twocat}}, \code{\link{phi.table}}, \code{\link{catdesc}},
 \code{\link{ggassoc_crosstab}}, \code{\link{ggassoc_assocplot}},
 \code{\link{ggassoc_phiplot}}, \code{\link{ggassoc_chiasmogram}}
}

\examples{
data(Movies)
ggassoc_bertin(Movies, ggplot2::aes(x = Country, y = Genre))
ggassoc_bertin(Movies, ggplot2::aes(x = Country, y = Genre),
 sort = "both", prop.width = TRUE, ncol = 3, add.rprop = TRUE)
}

\keyword{ multivariate }
\keyword{ aplot }
