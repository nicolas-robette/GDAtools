\name{ggadd_chulls}
\alias{ggadd_chulls}

\title{Convex hulls for a categorical supplementary variable}

\description{
Adds convex hulls for a categorical variable to a MCA cloud of individuals.
}

\usage{
ggadd_chulls(p, resmca, var, sel = 1:nlevels(var), axes = c(1,2), prop = 1, 
alpha = 0.2, label = TRUE, label.size = 5, legend = "right")
}

\arguments{
  \item{p}{\code{ggplot2} object with the cloud of individuals}
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}}
  \item{var}{Factor. The categorical variable used to plot chulls.}
  \item{sel}{numeric vector of indexes of the categories to plot (by default, ellipses are plotted for every categories)}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{prop}{proportion of all the points to be included in the hull (default is 1).}
  \item{alpha}{Numerical value from 0 to 1. Transparency of the polygon's fill. Default is O.2}
  \item{label}{Logical. Should the labels of the categories be plotted at the center of chulls ? Default is TRUE.}
  \item{label.size}{Size of the labels of the categories at the center of chulls. Default is 5.}
  \item{legend}{the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector). Default is right.}
}

\value{a \code{ggplot2} object}

\note{
Chulls are colored according to the categories of the variable, using the default \code{ggplot2} palette. The palette can be customized using any \code{scale_color_*} and \code{scale_fill_*} functions, such as \code{scale_color_brewer()} and \code{scale_fill_brewer()}, \code{scale_color_grey()} and \code{scale_fill_grey()}, or \code{scale_color_manual()} and \code{scale_fill_manual()}.
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{ggcloud_indiv}}, \code{\link{ggadd_supvar}}, \code{\link{ggadd_supvars}}, \code{\link{ggadd_kellipses}}, \code{\link{ggadd_ellipses}}, \code{\link{ggadd_interaction}}, \code{\link{ggsmoothed_supvar}}, \code{\link{ggadd_corr}}, \code{\link{ggadd_density}}
}

\examples{
# specific MCA of Taste example data set
data(Taste)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA",
          "Comedy.NA", "Crime.NA", "Animation.NA", "SciFi.NA", "Love.NA", 
          "Musical.NA")
mca <- speMCA(Taste[,1:11], excl = junk)
# hierarchical clustering 
# and partition of the individuals into 3 clusters
d <- dist(mca$ind$coord[, c(1,2)])
hca <- hclust(d, "ward.D2")
cluster <- factor(cutree(hca, 3))
# cloud of individuals
# with convex hulls for the clusters.
p <- ggcloud_indiv(mca, col = "black")
ggadd_chulls(p, mca, cluster)
}

\keyword{ aplot }
\keyword{ multivariate }
