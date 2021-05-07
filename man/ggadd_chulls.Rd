\name{ggadd_chulls}
\alias{ggadd_chulls}

\title{Adds convex hulls to a cloud of individuals}

\description{
Adds convex hulls for a categorical variable to a MCA cloud of individuals, using the ggplot2 framework.
}

\usage{
ggadd_chulls(p, resmca, var, sel=1:nlevels(var), axes=c(1,2),
col=NULL, alpha=0.2, label=TRUE, label.size=5, legend="right")
}

\arguments{
  \item{p}{ggplot object with the cloud of variables}
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}}
  \item{var}{Factor. The categorical variable used to plot ellipses.}
  \item{sel}{numeric vector of indexes of the categories to plot (by default, ellipses are plotted for every categories)}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{col}{Colors for the ellipses and labels of the categories. Can be the name of a palette from the RcolorBrewer package, 'bw' for a black and white palette (uses \code{scale_color_grey()}), a character vector of colors for a custom palette, or the name of a color for a single color. If NULL (default), the default palette of \code{ggplot2} is used.}
  \item{alpha}{Numerical value from 0 to 1. Transparency of the polygon's fill. Default is O.2}
  \item{label}{Logical. Should the labels of the categories be plotted at the center of ellipses ? Default is TRUE.}
  \item{label.size}{Size of the labels of the categories at the center of ellipses. Default is 5.}
  \item{legend}{the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector). Default is right.}
}

\value{a ggplot object}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{ggcloud_variables}}, \code{\link{ggcloud_indiv}}, \code{\link{ggadd_supvar}}, \code{\link{ggadd_corr}}, \code{\link{ggadd_interaction}}, \code{\link{ggadd_density}}, \code{\link{ggadd_kellipses}}, \code{\link{ggadd_ellipses}}
}

\examples{
## Performs a specific MCA on 'Taste' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## then performs hierarchical clustering and
## partitions the individuals into 3 clusters, 
## draws the cloud of individuals
## and adds convex hulls for the clusters.

data(Taste)
getindexcat(Taste)
mca <- speMCA(Taste[,1:11],excl=c(3,6,9,12,15,18,21,24,27,30,33))
d <- dist(mca$ind$coord[,c(1,2)])
hca <- hclust(d, "ward.D2")
cluster <- factor(cutree(hca, 3))
p <- ggcloud_indiv(mca, col='black')
ggadd_chulls(p, mca, cluster)
}