\name{plot.speMCA}

\alias{plot.speMCA}

\title{Plot of specific MCA}

\description{Plots a specific Multiple Correspondence Analysis (resulting from \code{\link{speMCA}} function), i.e. the clouds of individuals or categories.}

\usage{
\method{plot}{speMCA}(x, type = "v", axes = c(1,2), points = "all", col = "dodgerblue4", app = 0, \dots)
}

\arguments{
  \item{x}{object of class \code{speMCA}}
  \item{type}{character string: 'v' to plot the categories (default), 'i' to plot individuals' points, 'inames' to plot individuals' names}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot (c(1,2) is default)}
  \item{points}{character string. If 'all' all points are plotted (default); if 'besth' only those who contribute most to horizontal axis are plotted; if 'bestv' only those who contribute most to vertical axis are plotted; if 'besthv' only those who contribute most to horizontal or vertical axis are plotted; if 'best' only those who contribute most to the plane are plotted.}
  \item{col}{color for the points of the individuals or for the labels of the categories (default is 'dodgerblue4')}
  \item{app}{numerical value. If 0 (default), only the labels of the categories are plotted and their size is constant; if 1, only the labels are plotted and their size is proportional to the weights of the categories; if 2, points (triangles) and labels are plotted, and points size is proportional to the weight of the categories.}
  \item{\dots}{further arguments passed to or from other methods, such as cex, cex.main, ...}
}

\details{
A category is considered to be one of the most contributing to a given axis if its contribution is 
higher than the average contribution, i.e. 100 divided by the total number of categories.
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{speMCA}}, \code{\link{textvarsup}}, \code{\link{conc.ellipse}}
}

\examples{
# specific MCA of Music example data set
data(Music)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA")
mca <- speMCA(Music[,1:5], excl = junk)
# cloud of categories
plot(mca)
}

\keyword{ aplot }
\keyword{ multivariate }
