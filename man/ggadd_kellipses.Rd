\name{ggadd_kellipses}
\alias{ggadd_kellipses}

\title{Adds k-inertia ellipses to a cloud of individuals}

\description{
Adds k-inertia ellipses for a categorical variable to a MCA cloud of individuals, using the ggplot2 framework. By default, concentration ellipses are plotted.
}

\usage{
ggadd_kellipses(p, resmca, var, sel=1:nlevels(var), axes=c(1,2),
kappa=2, label=TRUE, label.size=3, col=NULL, size=0.5, points=TRUE, legend='right')
}

\arguments{
  \item{p}{ggplot object with the cloud of variables}
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}}
  \item{var}{Factor. The categorical variable used to plot ellipses.}
  \item{sel}{numeric vector of indexes of the categories to plot (by default, ellipses are plotted for every categories)}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{kappa}{numeric. The kappa value (i.e. "index") of the inertia ellipses. By default, kappa = 2, which means that concentration ellipses are plotted.}
  \item{label}{Logical. Should the labels of the categories be plotted at the center of ellipses ? Default is TRUE.}
  \item{label.size}{Size of the labels of the categories at the center of ellipses. Default is 3.}  
  \item{col}{Colors for the ellipses and labels of the categories. Can be the name of a palette from the RcolorBrewer package, 'bw' for a black and white palette (uses \code{scale_color_grey()}), a character vector of colors for a custom palette, or the name of a color for a single color. If NULL (default), the default palette of \code{ggplot2} is used.}
  \item{size}{Size of the lines of the ellipses. Default is 0.5.}
  \item{points}{If TRUE (default), the points are coloured according to their subcloud.}
  \item{legend}{the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector). Default is right.}
}

\value{a ggplot object}

\details{
If kappa=2, ellipses are called "concentration" ellipses and, for a normally shaped subcloud, contain 86.47 percents of the points of the subcloud. If kappa=1, ellipses are "indicator" ellipses and contain 39.35 percents of the points of the subcloud. If kappa=1.177, ellipses are "median" ellipses and contain 50 percents of the points of the subcloud.
This function has to be used after the cloud of individuals has been drawn.
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{ggcloud_variables}}, \code{\link{ggcloud_indiv}}, \code{\link{ggadd_supvar}}, \code{\link{ggadd_corr}}, \code{\link{ggadd_interaction}}, \code{\link{ggadd_density}}, \code{\link{ggadd_ellipses}}
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
ggadd_kellipses(p, mca, Music$Age)
}