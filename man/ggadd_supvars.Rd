\name{ggadd_supvars}
\alias{ggadd_supvars}

\title{Adds categorical supplementary variables to a cloud of variables}

\description{
Adds categorical supplementary variables to a MCA cloud of variables, using the ggplot2 framework.
}

\usage{
ggadd_supvars(p, resmca, vars, axes=c(1,2), palette = "khroma::bright",
shape=FALSE, prop=NULL, textsize=3, shapesize=6, vname=TRUE)
}

\arguments{
  \item{p}{ggplot object with the cloud of variables}
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}.}
  \item{vars}{A data frame of categorical supplementary variables. All these variables should be factors.}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{palette}{character string. Palette name, in the form packagename::palettename (cf \code{paletteer} package). If NULL, color argument is used. Default is "khroma::bright", a colorblind friendly palette.}
  \item{shape}{Logical. If TRUE, symbols are used in addition the the labels of categories. Default is FALSE.}
  \item{prop}{If NULL, the size of the labels (if shape=FALSE) or the shapes (otherwise) is constant. If 'n', the size is proportional the the weights of categories; if 'vtest1', the size is proportional to the test values of the categories on the first dimension of the plot; if 'vtest2', the size is proportional to the test values of the categories on the second dimension of the plot; if 'cos1', the size is proportional to the cosines of the categories on the first dimension of the plot; if 'cos2', the size is proportional to the cosines of the categories on the second dimension of the plot; if 'cos12', the size is proportional to the total cosines of the categories on the two dimensions of the plot.}
  \item{textsize}{Size of the labels of categories if shape is TRUE, or if shape=FALSE and prop=NULL. Default is 3.}
  \item{shapesize}{Size of the shapes if prop=NULL, maximum size of the shapes in other cases. Default is 6.}
  \item{vname}{Logical. If TRUE (default), the variable name is added as a prefix for the labels of the categories.}
}

\value{a ggplot object}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{varsups}}, \code{\link{ggcloud_variables}}, \code{\link{ggcloud_indiv}}, \code{\link{ggadd_ellipses}}, \code{\link{ggadd_corr}}, \code{\link{ggadd_interaction}}, \code{\link{ggadd_density}}
}

\examples{
## Performs a specific MCA on 'Music' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## draws the cloud of categories
## and adds Age as a supplementary variable.
data(Music)
getindexcat(Music[,1:5])
mca <- speMCA(Music[,1:5],excl=c(3,6,9,12,15))
p <- ggcloud_variables(mca, palette='lightgrey', shapes=FALSE)
ggadd_supvars(p, mca, Music[,6:9], prop = 'cos12')
}

\keyword{ aplot }
\keyword{ multivariate }
