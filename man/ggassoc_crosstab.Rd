\name{ggassoc_crosstab}

\alias{ggassoc_crosstab}

\title{Plots counts and associations of a crosstabulation}

\description{
For a cross-tabulation, plots the number of observations by using rectangles with proportional areas, and the phi measures of association between the categories with a diverging gradient of colour, using ggplot2.
}

\usage{
ggassoc_crosstab(data, mapping, maxphi=.8, axes.labs=TRUE, ticks.labs=TRUE)
}

\arguments{
  \item{data}{dataset to use for plot}
  \item{mapping}{aesthetic being used. It must specify x and y.}
  \item{maxphi}{numeric value, specifying the lower and upper limit of the scale for the color gradient filling the rectangles, i.e. for the phi measures of association. It corresponds to the maximum absolute value of phi one wants to represent in the plot.}
  \item{axes.labs}{Whether to display the labels of the axes, i.e. the names of x and y. Default is TRUE.}
  \item{ticks.labs}{Whether to display the labels of the categories of x and y. Default is TRUE.}  
}

\details{
The phi measure of association measures how much each combination of categories of x and y is over/under-represented.

Cramer's V measure of global association between x and y is displayed in upper-left corner of the plot.

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
