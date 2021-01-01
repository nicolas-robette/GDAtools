\name{ggcloud_variables}
\alias{ggcloud_variables}

\title{Plots MCA cloud of variables with ggplot2}

\description{
Plots a Multiple Correspondence Analysis cloud of variables, using ggplots functions.
}

\usage{
ggcloud_variables(resmca, axes=c(1,2), points='all', shapes=TRUE,
prop=NULL, textsize=3, shapesize=3, col=NULL, palette=NULL, alpha=1,
segment.alpha=0.5, vlab=TRUE, sep='.', legend='right')
}

\arguments{
  \item{resmca}{object of class \code{'MCA'}, \code{'speMCA'} or \code{'csMCA'}.}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{points}{character string. If 'all' all categories are plotted (default); if 'besth' only those 
                who contribute most to horizontal axis are plotted; if 'bestv' only those who contribute 
                most to vertical axis are plotted; if 'best' only those who contribute most to horizontal 
                or vertical axis are plotted.}
  \item{shapes}{Logical. Should shapes be plotted for categories (in addition to labels) ? Default is TRUE.}
  \item{prop}{If NULL, the size of the labels (if shapes=FALSE) or the shapes (if shapes=TRUE) is constant. If 'n', the size is proportional the the weights of categories; if 'ctr1', the size is proportional to the contributions of the categories on the first dimension of the plot; if 'ctr2', the size is proportional to the contributions of the categories on the second dimension of the plot; if 'ctr.cloud', the size is proportional to the total contributions of the categories on the whole cloud; if 'cos1', the size is proportional to the cosines of the categories on the first dimension of the plot; if 'cos2', the size is proportional to the cosines of the categories on the second dimension of the plot; if 'cos12', the size is proportional to the total cosines of the categories on the two dimensions of the plot.}
  \item{textsize}{Size of the labels of categories if shapes=TRUE, or if shapes=FALSE and prop=NULL.               Default is 3.}
  \item{shapesize}{Size if the shapes of categories if shapes=TRUE and prop=FALSE. Default is 3.}
  \item{col}{Character. A unique color for the shapes and labels of the categories. Default is NULL, which means a palette will be used instead of a unique color (see \code{palette} argument).}
  \item{palette}{Character string or character vector. Only used if \code{col} is NULL. Colors for the shapes and labels of the categories, differentiated by variable. Can be the name of a palette from the \code{RcolorBrewer} package, "bw" for a black and white palette (uses \code{scale_color_grey()}) or a character vector of colors for a custom palette. If NULL (default), the default palette of \code{ggplot2} is used.}
  \item{alpha}{Transparency of the shapes and labels of categories. Default is 1.}
  \item{segment.alpha}{Transparency of the line segment beside labels of categories. Default is 0.5.}
  \item{vlab}{Logical. Should the variable names be used as a prefix for the labels of the categories.                    Default is TRUE.}
  \item{sep}{Character string used as a separator if vlab=TRUE.}
  \item{legend}{the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector). Default is right.}
}

\value{a ggplot object}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Anton Perdoncin, Nicolas Robette}

\seealso{
\code{\link{ggcloud_indiv}}, \code{\link{ggadd_supvar}}, \code{\link{ggadd_ellipses}}, \code{\link{ggadd_interaction}}
}

\examples{
## Performs a specific MCA on 'Music' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## and then draws the cloud of categories.
data(Music)
getindexcat(Music[,1:5])
mca <- speMCA(Music[,1:5],excl=c(3,6,9,12,15))
ggcloud_variables(mca)
ggcloud_variables(mca, points='best', prop='n', palette='Set2')
}

\keyword{ aplot }
\keyword{ multivariate }
