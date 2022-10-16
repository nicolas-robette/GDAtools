\name{ggassoc_phiplot}

\alias{ggassoc_phiplot}

\title{Bar plot of phi measures of association of a crosstabulation}

\description{
For a cross-tabulation, plots the phi measures of association with bars of varying height, using ggplot2.
}

\usage{
ggassoc_phiplot(data, mapping, measure="phi", max.asso=NULL,
sort="none")
}

\arguments{
  \item{data}{dataset to use for plot}
  \item{mapping}{aesthetic being used. It must specify x and y.}
  \item{measure}{character. The measure of association used for filling the rectangles. Can be "phi" for phi coefficient (default), "or" for odds ratios, "std.residuals" for standardized Pearson residuals or "pem" for local percentages of maximum deviation from independence.}
  \item{max.asso}{numeric value, specifying the lower and upper limit of the scale for the color gradient filling the rectangles, i.e. for the measures of association. It corresponds to the maximum absolute value of association one wants to represent in the plot. If NULL (default), the limit is automatically adjusted to the data.}
  \item{sort}{character. If "both", rows and columns are sorted according to the first factor of a correspondence analysis of the contingency table. If "x", only rows are sorted. If "y", only columns are sorted. If "none" (default), no sorting is done.}
}

\details{
The measure of association measures how much each combination of categories of x and y is over/under-represented.
The bars vary in width according to the number of observations in the categories of the column variable. They vary in height according to the measure of association. Bars are black if the association is positive and white if it is negative.

This function can be used as a high-level plot with \code{ggduo} and \code{ggpairs} functions of the \code{GGally} package. 
}

\value{
a ggplot object
}

\references{
Cibois Philippe, 2004, \emph{Les écarts à l'indépendance. Techniques simples pour analyser des données d'enquêtes}, Collection "Méthodes quantitatives pour les sciences sociales"
}

\author{Nicolas Robette}

\seealso{
 \code{\link{assoc.twocat}}, \code{\link{phi.table}}, \code{\link{catdesc}},
 \code{\link{assoc.yx}}, \code{\link{darma}}, \code{\link{ggassoc_crosstab}}, \code{\link[GGally]{ggpairs}}
}

\examples{
data(Taste)
ggassoc_phiplot(data=Taste, mapping=ggplot2::aes(x=Age, y=Classical))
}

\keyword{ multivariate }
\keyword{ aplot }
