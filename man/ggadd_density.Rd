\name{ggadd_density}
\alias{ggadd_density}

\title{Density plot of a supplementary variable}

\description{
For a given category of a supplementary variable, adds a layer representing the density of points to the cloud of individuals, either with contours or areas.
}

\usage{
ggadd_density(p, resmca, var, cat = levels(var)[1], axes = c(1,2),
density = "contour", col.contour = "darkred", pal.area = "viridis",
alpha.area = 0.2, ellipse = FALSE)
}

\arguments{
  \item{p}{\code{ggplot2} object with the cloud of variables}
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}}
  \item{var}{factor or numerical vector. The supplementary variable to be plotted.}
  \item{cat}{character string. The category of \code{var} to plot (by default, the first level of \code{var} is plotted). Only used if var is a factor.}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{density}{If "contour" (default), density is plotted with contours. If "area", density is plotted with areas.}
  \item{col.contour}{character string. The color of the contours.}
  \item{pal.area}{character string. The name of a viridis palette for areas.}
  \item{alpha.area}{numeric. Transparency of the areas. Default is 0.2.}
  \item{ellipse}{logical. If TRUE, a concentration ellipse is added.}
}

\value{a \code{ggplot2} object}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{ggcloud_indiv}}, \code{\link{ggadd_supvar}}, \code{\link{ggadd_supvars}}, \code{\link{ggadd_kellipses}}, \code{\link{ggadd_ellipses}}, \code{\link{ggadd_interaction}}, \code{\link{ggsmoothed_supvar}}, \code{\link{ggadd_chulls}}, \code{\link{ggadd_corr}}
}

\examples{
# specific MCA of Taste example data set
data(Taste)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA",
          "Comedy.NA", "Crime.NA", "Animation.NA", "SciFi.NA", "Love.NA", 
          "Musical.NA")
mca <- speMCA(Taste[,1:11], excl = junk)
p <- ggcloud_indiv(mca, col='lightgrey')
# density plot for Age = 50+ (with contours)
ggadd_density(p, mca, var = Taste$Age, cat = "50+")
# density plot for Age = 50+ (with contours)
ggadd_density(p, mca, var = Taste$Age, cat = "50+", density = "area")
}

\keyword{ aplot }
\keyword{ multivariate }
