\name{ggadd_ellipses}
\alias{ggadd_ellipses}

\title{Adds confidence ellipses to a cloud of individuals}

\description{
Adds confidence ellipses for a categorical variable to a MCA cloud of individuals, using the ggplot2 framework.
}

\usage{
ggadd_ellipses(p, resmca, var, sel=1:nlevels(var), axes=c(1,2),
level=0.05, label=TRUE, label.size=3, col=NULL, size=0.5, points=TRUE, legend='right')
}

\arguments{
  \item{p}{ggplot object with the cloud of variables}
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}}
  \item{var}{Factor. The categorical variable used to plot ellipses.}
  \item{sel}{numeric vector of indexes of the categories to plot (by default, ellipses are plotted for every categories)}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{level}{The level at which to draw an ellipse (see \code{stat_ellipse}). Default is 0.05, which means 95 percents confidence ellipses are plotted.}
  \item{label}{Logical. Should the labels of the categories be plotted at the center of ellipses ? Default is TRUE.}
  \item{label.size}{Size of the labels of the categories at the center of ellipses. Default is 3.}
  \item{col}{Colors for the ellipses and labels of the categories. Can be the name of a palette from the RcolorBrewer package, 'bw' for a black and white palette (uses \code{scale_color_grey()}), a character vector of colors for a custom palette, or the name of a color for a single color. If NULL (default), the default palette of \code{ggplot2} is used.}
  \item{size}{Size of the lines of the ellipses. Default is 0.5.}
  \item{points}{If TRUE (default), the points are coloured according to their subcloud.}
  \item{legend}{the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector). Default is right.}
}

\value{a ggplot object}

\details{
A confidence ellipse aims at measuring how the "true" mean point of a category differs from its observed mean point. This is achieved by constructing a confidence zone around the observed mean point. If we choose a conventional level alpha (e.g. 0.05), a (1 - alpha) (e.g. 95 percents) confidence zone is defined as the set of possible mean points that are not significantly different from the observed mean point.
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{ggcloud_variables}}, \code{\link{ggcloud_indiv}}, \code{\link{ggadd_supvar}}, \code{\link{ggadd_corr}}, \code{\link{ggadd_interaction}}, \code{\link{ggadd_density}}, \code{\link{ggadd_kellipses}}
}

\examples{
## Performs a specific MCA on 'Music' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## draws the cloud of categories
## and adds confidence ellipses for Age.
data(Music)
getindexcat(Music[,1:5])
mca <- speMCA(Music[,1:5],excl=c(3,6,9,12,15))
p <- ggcloud_indiv(mca, col='lightgrey')
ggadd_ellipses(p, mca, Music$Age)
}