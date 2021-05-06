\name{ggadd_interaction}
\alias{ggadd_interaction}

\title{
Adds the interaction between two categorical supplementary variables to a cloud of variables
}

\description{
Adds the interaction between two categorical supplementary variables to a MCA cloud of variables, using the ggplot2 framework.
}

\usage{
ggadd_interaction(p, resmca, v1, v2, sel1=1:nlevels(v1), sel2=1:nlevels(v2),
axes=c(1,2), col=NULL, textsize=5, legend='right')
}

\arguments{
  \item{p}{ggplot object with the cloud of variables}
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}.}
  \item{v1}{Factor. The first categorical supplementary variable.}
  \item{v2}{Factor. The second categorical supplementary variable.}
  \item{sel1}{Numeric vector of indexes of the categories of the first supplementary variable to be used in interaction. By default, every categories are used.}
  \item{sel2}{Numeric vector of indexes of the categories of the second supplementary variable to be used in interaction. By default, every categories are used.}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{col}{Colors for the labels of the categories. Can be the name of a palette from the RcolorBrewer package, 'bw' for a black and white palette (uses \code{scale_color_grey()}), a character vector of colors for a custom palette, or the name of a color for a single color. If NULL (default), the default palette of \code{ggplot2} is used.}
  \item{textsize}{Size of the labels of categories. Default is 5.}
  \item{legend}{the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector). Default is right.}
}

\value{a ggplot object}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{ggcloud_variables}}, \code{\link{ggadd_supvar}}, \code{\link{ggcloud_indiv}}, \code{\link{ggadd_corr}}, \code{\link{ggadd_ellipses}}, \code{\link{ggadd_density}}
}

\examples{
## Performs a specific MCA on 'Taste' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## draws the cloud of categories
## and adds the interaction between Gender and Age.
data(Taste)
getindexcat(Taste[,1:11])
mca <- speMCA(Taste[,1:11],excl=c(3,6,9,12,15,18,21,24,27,30,33))
p <- ggcloud_variables(mca, col='lightgrey', shapes=FALSE)
ggadd_interaction(p, mca, Taste$Gender, Taste$Age)
}

\keyword{ aplot }
\keyword{ multivariate }
