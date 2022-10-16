\name{ggassoc_crosstab}

\alias{ggassoc_crosstab}

\title{Proportional area plot}

\description{
For a cross-tabulation, plots the observed (or expected) frequencies by using rectangles with proportional areas, and the measures of local association between the categories with a diverging gradient of colour, using ggplot2.
}

\usage{
ggassoc_crosstab(data, mapping, size = "observed", 
measure = "phi", max.asso = NULL, sort = "none", 
palette = "PRGn", direction = 1)
}

\arguments{
  \item{data}{dataset to use for plot}
  \item{mapping}{aesthetic being used. It must specify x and y.}
  \item{size}{character. If "observed" (default), areas are proportional to observed frequencies. Il "expected", they are proportional to expected frequencies.}
  \item{measure}{character. The measure of association used for filling the rectangles. Can be "phi" for phi coefficient (default), "or" for odds ratios, "residuals" for Pearson residuals, "std.residuals" for standardized Pearson residuals or "pem" for local percentages of maximum deviation from independence.}
  \item{max.asso}{numeric value, specifying the lower and upper limit of the scale for the color gradient filling the rectangles, i.e. for the measures of association. It corresponds to the maximum absolute value of association one wants to represent in the plot. If NULL (default), the limit is automatically adjusted to the data.}
  \item{sort}{character. If "both", rows and columns are sorted according to the first factor of a correspondence analysis of the contingency table. If "x", only rows are sorted. If "y", only columns are sorted. If "none" (default), no sorting is done.}
  \item{palette}{character. Name of a ColorBrewer diverging palette. Default is PRGn.}
  \item{direction}{Sets the order of colours in the scale. If 1, the default, colours are as output by RColorBrewer::brewer.pal(). If -1, the order of colours is reversed.}
}

\details{
The measure of local association measures how much each combination of categories of x and y is over/under-represented.

The areas of the rectangles are proportional to observed or expected frequencies. Their color shading varies according to the measure of association.

This function can be used as a high-level plot with \code{ggduo} and \code{ggpairs} functions of the \code{GGally} package. 
}

\value{
a ggplot object
}

\author{Nicolas Robette}

\seealso{
 \code{\link{assoc.twocat}}, \code{\link{phi.table}}, \code{\link{catdesc}},
 \code{\link{assoc.yx}}, \code{\link{darma}}, \code{\link{ggassoc_phiplot}}, \code{\link[GGally]{ggpairs}}
}

\examples{
data(Taste)
ggassoc_crosstab(data=Taste, mapping=ggplot2::aes(x=Age, y=Classical))
}

\keyword{ multivariate }
\keyword{ aplot }
