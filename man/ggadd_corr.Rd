\name{ggadd_corr}
\alias{ggadd_corr}

\title{Adds a heatmap of under/over-representation of a supplementary variable to a cloud of individuals}

\description{
Adds a heatmap representing the correlation coefficients to a MCA cloud of individuals, for a numerical supplementary variable or one category of a categorical supplementary variable, using the ggplot2 framework.
}

\usage{
ggadd_corr(p, resmca, var, cat=levels(var)[1], axes=c(1,2),
xbins=20, ybins=20, min.n=1, pal="RdYlBu", limits=NULL, legend="right")
}

\arguments{
  \item{p}{ggplot object with the cloud of variables}
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}}
  \item{var}{factor or numerical vector. The supplementary variable used for the heatmap.}
  \item{cat}{character string. numeric vector of indexes of the categories to plot (by default, ellipses are plotted for every categories). Only used if var is a factor.}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{xbins}{integer. Number of bins in the x axis. Default is 20.}
  \item{ybins}{integer. Number of bins in the y axis. Default is 20.}
  \item{min.n}{integer. Minimal number of points for a tile to be drawn. By default, every tiles are drawn.}
  \item{pal}{character string. Name of a diverging ColorBrewer palette. Default is "RdYlBu".}
  \item{limits}{numerical vector of length 2. Lower and upper limits of the correlation coefficients for the color scale. Should be centered around 0 for a better view of under/over-representations (for example c(-0.2,0.2)). By default, the maximal absolute value of the correlation coefficients is used.}
  \item{legend}{the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector). Default is right.}
}

\value{a ggplot object}

\details{
For each tile of the heatmap, a correlation coefficient is computed between the supplementary variable and the fact of belonging to the tile. This gives a view of the under/over-representation of the supplementary variable according to the position in the cloud of individuals.
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{ggcloud_variables}}, \code{\link{ggcloud_indiv}}, \code{\link{ggadd_supvar}}, \code{\link{ggadd_interaction}}, \code{\link{ggadd_ellipses}}, \code{\link{ggadd_density}}
}

\examples{
## Performs a specific MCA on 'Taste' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## draws the cloud of categories
## and adds a correlation heatmap for Age=50+.
data(Taste)
getindexcat(Taste[,1:11])
mca <- speMCA(Taste[,1:11],excl=c(3,6,9,12,15,18,21,24,27,30,33))
p <- ggcloud_indiv(mca, col='lightgrey')
ggadd_corr(p, mca, var=Taste$Age, cat="50+", xbins=10, ybins=10)
}