\name{ggadd_supvars}
\alias{ggadd_supvars}

\title{Plot of categorical supplementary variables}

\description{
Adds categorical supplementary variables to a MCA cloud of variables.
}

\usage{
ggadd_supvars(p, resmca, vars, excl = NULL, points = "all", min.cos2 = 0.1,
axes = c(1,2), col = NULL,
shapes = FALSE, prop = NULL, textsize = 3, shapesize = 6,
vlab = TRUE, vname = NULL)
}

\arguments{
  \item{p}{\code{ggplot2} object with the cloud of variables}
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}.}
  \item{vars}{A data frame of categorical supplementary variables. All these variables should be factors.}
  \item{excl}{character vector of supplementary categories to exclude from the plot, specified in the form "namevariable.namecategory" (for instance "Gender.Men"). If NULL (default), all the supplementary categories are plotted.}
  \item{points}{character string. If 'all' all categories are plotted (default); if 'besth' only those with a minimum squared cosine on horizontal axis are plotted; if 'bestv' only those with a minimum squared cosine on vertical axis are plotted; if 'besthv' only those with a minimum squared cosine on horizontal or vertical axis are plotted; if 'best' only those with a minimum squared cosine on the plane are plotted.}
  \item{min.cos2}{numerical value. The minimal squared cosine if 'points' argument is different from 'all'. Default }
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{col}{character string. Color name for the labels (and the shapes if \code{shapes=TRUE}) of the categories. If NULL, the default palette of \code{ggplot2} is used, with one color per variable.}
  \item{shapes}{Logical. If TRUE, symbols are used in addition to the labels of categories. Default is FALSE.}
  \item{prop}{If NULL, the size of the labels (if \code{shapes=FALSE}), or of the labels and the shapes (if \code{shapes=TRUE}) is constant. If 'n', the size is proportional the the weights of categories; if 'vtest1', the size is proportional to the test values of the categories on the first dimension of the plot; if 'vtest2', the size is proportional to the test values of the categories on the second dimension of the plot; if 'cos1', the size is proportional to the cosines of the categories on the first dimension of the plot; if 'cos2', the size is proportional to the cosines of the categories on the second dimension of the plot; if 'cos12', the size is proportional to the total cosines of the categories on the two dimensions of the plot.}
  \item{textsize}{Size of the labels of categories if \code{shapes} is TRUE, or if \code{shapes} is FALSE and \code{prop} is NULL. Default is 3.}
  \item{shapesize}{Size of the shapes if \code{prop=NULL}, maximum size of the shapes in other cases. Default is 6.}
  \item{vlab}{Logical. If TRUE (default), the variable name is added as a prefix for the labels of the categories.}
  \item{vname}{deprecated, use vlab instead}
}

\value{a \code{ggplot2} object}

\note{
Shapes and labels are colored according to the categories of the variable, using the default \code{ggplot2} palette. The palette can be customized using any \code{scale_color_*} function, such as \code{scale_color_brewer()}, \code{scale_color_grey()} or \code{scale_color_manual()}.
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{ggcloud_variables}}, \code{\link{ggadd_supvar}}, \code{\link{ggadd_ellipses}}, \code{\link{ggadd_kellipses}}, \code{\link{ggadd_density}}, \code{\link{ggadd_interaction}}, \code{\link{ggsmoothed_supvar}}, \code{\link{ggadd_chulls}}, \code{\link{ggadd_corr}}
}

\examples{
# specific MCA of Music example data set
data(Music)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA")
mca <- speMCA(Music[,1:5], excl = junk)
# adds several supplementary variables
# onto the cloud of variables
p <- ggcloud_variables(mca, col = "lightgrey", shapes = FALSE)
ggadd_supvars(p, mca, Music[, c("Gender","Age")])
# the same, excluding men
ggadd_supvars(p, mca, Music[, c("Gender","Age")], excl = "Gender.Men")
# the same, keeping only categories
# with cos2 >= 0.001 for dimension 1
ggadd_supvars(p, mca, Music[, c("Gender","Age")], points = "besth", min.cos2 = 0.001)
}

\keyword{ aplot }
\keyword{ multivariate }
