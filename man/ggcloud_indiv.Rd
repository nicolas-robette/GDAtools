\name{ggcloud_indiv}
\alias{ggcloud_indiv}

\title{Plots MCA cloud of individuals}

\description{
Plots a Multiple Correspondence Analysis cloud of individuals, using ggplots functions.
}

\usage{
ggcloud_indiv(resmca, type='i', points='all', axes=1:2, 
col='dodgerblue4', palette='Set2', alpha=0.6, repel=FALSE)
}

\arguments{
  \item{resmca}{object of class \code{'MCA'}, \code{'speMCA'} or \code{'csMCA'}.}
  \item{type}{If 'i', points are plotted; if 'inames', labels of individuals are plotted.}
  \item{points}{character string. If 'all' all points are plotted (default); if 'besth' only those 
                who contribute most to horizontal axis are plotted; if 'bestv' only those who contribute 
                most to vertical axis are plotted; if 'best' only those who contribute most to horizontal 
                or vertical axis are plotted.}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{col}{If a factor, points or labels are colored according to their category regarding this factor. If a string with color name, every points or labels have the same color. Default is 'dodgerblue4'.}
  \item{palette}{If \code{col} is a factor, can be the name of a \code{RColorBrewer} palette, 'bw' for a black and white palette (uses \code{scale_color_grey()}) or a character vector with a custom palette. If NULL, \code{ggplot2} palette is used. Default is 'Set2'.}
  \item{alpha}{Transparency of the points or labels of individuals. Default is 0.6.}
  \item{repel}{Logical. When type='inames', should labels of individuals be repeled ? Default is FALSE.}
}

\value{a ggplot object}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Anton Perdoncin, Nicolas Robette}

\seealso{
\code{\link{ggcloud_variables}}, \code{\link{ggadd_ellipses}}, \code{\link{ggadd_supvar}}, \code{\link{ggadd_interaction}}
}

\examples{
## Performs a specific MCA on 'Music' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## and then draws the cloud of individuals.
data(Music)
getindexcat(Music[,1:5])
mca <- speMCA(Music[,1:5],excl=c(3,6,9,12,15))
ggcloud_indiv(mca)
ggcloud_indiv(mca, col=Music$Gender, palette='Set2')
}

\keyword{ aplot }
\keyword{ multivariate }