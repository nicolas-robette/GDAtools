\name{ggadd_supvar}
\alias{ggadd_supvar}

\title{Plot of a categorical supplementary variable}

\description{
Adds a categorical supplementary variable to a MCA cloud of variables.
}

\usage{
ggadd_supvar(p, resmca, var, sel = 1:nlevels(var), axes = c(1,2),
col = "black", shape = 1, prop = NULL, textsize = 3, shapesize = 6,
segment = FALSE, vname = NULL)
}

\arguments{
  \item{p}{\code{ggplot2} object with the cloud of variables}
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}.}
  \item{var}{Factor. The categorical supplementary variable. It does not need to have been used at the MCA step.}
  \item{sel}{Numeric vector of indexes of the categories of the supplementary variable to be added to the plot. By default, labels are plotted for every categories.}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{col}{Character. Color of the shapes and labels of the categories. Default is black.}
  \item{shape}{Symbol to be used in addition the the labels of categories (default is 1). If NULL, only labels are plotted.}
  \item{prop}{If NULL, the size of the labels (if shape=NULL) or the shapes (otherwise) is constant. If 'n', the size is proportional the the weights of categories; if 'vtest1', the size is proportional to the test values of the categories on the first dimension of the plot; if 'vtest2', the size is proportional to the test values of the categories on the second dimension of the plot; if 'cos1', the size is proportional to the cosines of the categories on the first dimension of the plot; if 'cos2', the size is proportional to the cosines of the categories on the second dimension of the plot; if 'cos12', the size is proportional to the total cosines of the categories on the two dimensions of the plot.}
  \item{textsize}{Size of the labels of categories if shape is not NULL, or if shape=NULL and prop=NULL. Default is 3.}
  \item{shapesize}{Size of the shapes if prop=NULL, maximum size of the shapes in other cases. Default is 6.}
  \item{segment}{Logical. Should one add lines between categories ? Default is FALSE.}
  \item{vname}{A character string to be used as a prefix for the labels of the categories. If NULL (default), no prefix is added.}
}

\value{a \code{ggplot2} object}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{ggcloud_variables}}, \code{\link{ggadd_supvars}}, \code{\link{ggadd_ellipses}}, \code{\link{ggadd_kellipses}}, \code{\link{ggadd_density}}, \code{\link{ggadd_interaction}}, \code{\link{ggsmoothed_supvar}}, \code{\link{ggadd_chulls}}, \code{\link{ggadd_corr}}
}

\examples{
# specific MCA of Music example data set
data(Music)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA")
mca <- speMCA(Music[,1:5], excl = junk)
# adds Age as a supplementary variable
# onto the cloud of variables
p <- ggcloud_variables(mca, col = "lightgrey", shapes = FALSE)
ggadd_supvar(p, mca, Music$Age, segment = TRUE)
}

\keyword{ aplot }
\keyword{ multivariate }
