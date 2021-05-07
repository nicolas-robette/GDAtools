\name{ggadd_density}
\alias{ggadd_density}

\title{Adds a density layer to the cloud of individuals for a category of a supplementary variable}

\description{
For a given category of a supplementary variable, adds a layer representing the density of points to the cloud of individuals, either with contours or areas.
}

\usage{
ggadd_density(p, resmca, var, cat=levels(var)[1], axes=c(1,2),
density="contour", col.contour="darkred", pal.area="viridis", alpha.area=0.2,
ellipse=FALSE, col.ellipse="black")
}

\arguments{
  \item{p}{ggplot object with the cloud of variables}
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}}
  \item{var}{factor or numerical vector. The supplementary variable used for the heatmap.}
  \item{cat}{character string. numeric vector of indexes of the categories to plot (by default, ellipses are plotted for every categories)}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{density}{If "contour", density is plotted with contours. If "area", density is plotted with areas.}
  \item{col.contour}{character string. The color of the contours.}
  \item{pal.area}{character string. The name of a viridis palette for areas.}
  \item{alpha.area}{numeric. Transparency of the areas. Default is 0.2.}
  \item{ellipse}{logical. If TRUE, a concentration ellipse is added.}
  \item{col.ellipse}{character string. The color of the ellipse. Default is black.}
}

\value{a ggplot object}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{ggcloud_variables}}, \code{\link{ggcloud_indiv}}, \code{\link{ggadd_supvar}}, \code{\link{ggadd_interaction}}, \code{\link{ggadd_ellipses}}, \code{\link{ggadd_corr}}
}

\examples{
## Performs a specific MCA on 'Taste' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## draws the cloud of categories
## and adds a density layer for Age=50+.
data(Taste)
getindexcat(Taste[,1:11])
mca <- speMCA(Taste[,1:11],excl=c(3,6,9,12,15,18,21,24,27,30,33))
p <- ggcloud_indiv(mca, col='lightgrey')

# density layer with contours
ggadd_density(p, mca, var=Taste$Age, cat="50+")

# density layer with areas
ggadd_density(p, mca, var=Taste$Age, cat="50+", density="area")
}