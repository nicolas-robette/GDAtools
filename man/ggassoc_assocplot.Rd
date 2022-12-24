\name{ggassoc_assocplot}

\alias{ggassoc_assocplot}

\title{Association plot}

\description{
For a cross-tabulation, plots measures of local association with bars of varying height and width, using ggplot2.
}

\usage{
ggassoc_assocplot(data, mapping, measure="std.residuals", max.asso=NULL,
sort="none", palette = "PRGn", direction = 1)
}

\arguments{
  \item{data}{dataset to use for plot}
  \item{mapping}{aesthetic being used. It must specify x and y.}
  \item{measure}{character. The measure of association used for filling the rectangles. Can be "phi" for phi coefficient, "or" for odds ratios, "std.residuals" (default) for standardized (i.e. Pearson) residuals, "adj.residuals" for adjusted standardized residuals or "pem" for local percentages of maximum deviation from independence.}
  \item{max.asso}{numeric value, specifying the lower and upper limit of the scale for the color gradient filling the rectangles, i.e. for the measures of association. It corresponds to the maximum absolute value of association one wants to represent in the plot. If NULL (default), the limit is automatically adjusted to the data.}
  \item{sort}{character. If "both", rows and columns are sorted according to the first factor of a correspondence analysis of the contingency table. If "x", only rows are sorted. If "y", only columns are sorted. If "none" (default), no sorting is done.}
  \item{palette}{character. Name of a ColorBrewer diverging palette. Default is PRGn.}
  \item{direction}{Sets the order of colours in the scale. If 1, the default, colours are as output by RColorBrewer::brewer.pal(). If -1, the order of colours is reversed.}
}

\details{
The measure of local association measures how much each combination of categories of x and y is over/under-represented.

The bars vary in width according to the square root of the expected frequency. They vary in height and color shading according to the measure of association. If the measure chosen is "residuals" (Pearson's residuals), as in the original association plot from Cohen and Friendly, the area of the bar is proportional to the difference in observed and expected frequencies.

This function can be used as a high-level plot with \code{ggduo} and \code{ggpairs} functions of the \code{GGally} package. 
}

\value{
a ggplot object
}

\references{
Cohen, A. (1980), On the graphical display of the significant components in a two-way contingency table. \emph{Communications in Statistics—Theory and Methods}, 9, 1025–1041. doi:10.1080/03610928008827940.

Friendly, M. (1992), Graphical methods for categorical data. \emph{SAS User Group International Conference Proceedings}, 17, 190–200. http://datavis.ca/papers/sugi/sugi17.pdf 
}

\author{Nicolas Robette}

\seealso{
 \code{\link{assoc.twocat}}, \code{\link{phi.table}}, \code{\link{catdesc}},
 \code{\link{assoc.yx}}, \code{\link{darma}}, \code{\link{ggassoc_crosstab}}, \code{\link[GGally]{ggpairs}}
}

\examples{
data(Taste)
ggassoc_assocplot(data=Taste, mapping=ggplot2::aes(x=Age, y=Classical))
}

\keyword{ multivariate }
\keyword{ aplot }
