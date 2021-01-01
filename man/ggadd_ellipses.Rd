\name{ggadd_ellipses}
\alias{ggadd_ellipses}

\title{Adds concentration ellipses to a MCA cloud of individuals with ggplot2}

\description{
Adds concentration ellipses for a categorical variable to a MCA cloud of individuals, using the ggplot2 framework.
}

\usage{
ggadd_ellipses(p, resmca, var, sel=1:nlevels(var), axes=c(1,2),
label=TRUE, col=NULL, legend='right', level=0.86, alpha=0.5)
}

\arguments{
  \item{p}{ggplot object with the cloud of variables}
  \item{resmca}{object of class \code{'MCA'}, \code{'speMCA'} or \code{'csMCA'}.}
  \item{var}{Factor. The categorical variable used to plot ellipses.}
  \item{sel}{numeric vector of indexes of the categories to plot (by default, ellipses are plotted for every categories)}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{label}{Logical. Should the labels of teh categories be plotted at the center of ellipses ? Default is TRUE.}
  \item{col}{Colors for the ellipses and labels of the categories. Can be the name of a palette from the RcolorBrewer package, 'bw' for a black and white palette (uses \code{scale_color_grey()}), a character vector of colors for a custom palette, or the name of a color for a single color. If NULL (default), the default palette of \code{ggplot2} is used.}
  \item{legend}{the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector). Default is right.}
  \item{level}{The level at which to draw an ellipse (see \code{stat_ellipse}). Default is 0.86.}
  \item{alpha}{Transparency of the points of individuals. Default is 0.5.}
}

\value{a ggplot object}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{ggcloud_variables}}, \code{\link{ggcloud_indiv}}, \code{\link{ggadd_supvar}}, \code{\link{ggadd_interaction}}
}

\examples{
## Performs a specific MCA on 'Music' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## draws the cloud of categories
## and adds concentration ellipses for Age.
data(Music)
getindexcat(Music[,1:5])
mca <- speMCA(Music[,1:5],excl=c(3,6,9,12,15))
p <- ggcloud_indiv(mca, col='lightgrey')
ggadd_ellipses(p, mca, Music$Age)
}