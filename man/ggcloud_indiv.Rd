\name{ggcloud_indiv}
\alias{ggcloud_indiv}

\title{Plots MCA cloud of individuals with ggplot2}

\description{
Plots a Multiple Correspondence Analysis cloud of individuals, using ggplots functions.
}

\usage{
ggcloud_indiv(resmca, type='i', points='all', axes=1:2, 
col='dodgerblue4', palette='Set2', size = 0.5, alpha=0.6, repel=FALSE,
density=NULL, col.contour="darkred", hex.bins=50, hex.pal="viridis")
}

\arguments{
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}}
  \item{type}{If 'i', points are plotted; if 'inames', labels of individuals are plotted.}
  \item{points}{character string. If 'all' all points are plotted (default); if 'besth' only those 
                who contribute most to horizontal axis are plotted; if 'bestv' only those who contribute 
                most to vertical axis are plotted; if 'best' only those who contribute most to horizontal 
                or vertical axis are plotted.}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{col}{If a factor, points or labels are colored according to their category regarding this factor. If a string with color name, every points or labels have the same color. Default is 'dodgerblue4'.}
  \item{palette}{If \code{col} is a factor, can be the name of a \code{RColorBrewer} palette, 'bw' for a black and white palette (uses \code{scale_color_grey()}) or a character vector with a custom palette. If NULL, \code{ggplot2} palette is used. Default is 'Set2'.}
  \item{size}{Size of the points or labels of individuals. Default is 0.5.}
  \item{alpha}{Transparency of the points or labels of individuals. Default is 0.6.}
  \item{repel}{Logical. When type='inames', should labels of individuals be repeled ? Default is FALSE.}
  \item{density}{If NULL (default), no density layer is added. If "contour", density is plotted with contours. If "hex", density is plotted with hexagon bins.}
  \item{col.contour}{character string. The color of the contours. Only used if density="contour".}
  \item{hex.bins}{integer. The number of bins in both vertical and horizontal directions. Only used if density="hex".}
  \item{hex.pal}{character string. The name of a viridis palette for hexagon bins. Only used if density="hex".}
}

\value{a ggplot object}

\details{
Sometimes the dots are too many and overlap. It is then difficult to get an accurate idea of the distribution of the cloud of individuals. The "density" option allows you to add an additional layer to represent the density of points in the plane, in the form of contours or hexagonal areas.
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Anton Perdoncin, Nicolas Robette}

\seealso{
\code{\link{ggcloud_variables}}, \code{\link{ggadd_ellipses}}, \code{\link{ggadd_supvar}}, \code{\link{ggadd_corr}}, \code{\link{ggadd_interaction}}, \code{\link{ggadd_density}}
}

\examples{
## Performs a specific MCA on 'Taste' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## and then draws the cloud of individuals.
data(Taste)
getindexcat(Taste[,1:11])
mca <- speMCA(Taste[,1:11],excl=c(3,6,9,12,15,18,21,24,27,30,33))
ggcloud_indiv(mca)

# Points are colored according to gender
ggcloud_indiv(mca, col=Taste$Gender, palette='Set2')

# A density layer of contours is added
ggcloud_indiv(mca, density="contour")

# A density layer of hexagon bins is added
ggcloud_indiv(mca, density="hex", hex.bin=10)

}

\keyword{ aplot }
\keyword{ multivariate }