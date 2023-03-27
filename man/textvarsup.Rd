\name{textvarsup}

\alias{textvarsup}

\title{Plot of a categorical supplementary variable}

\description{Adds a categorical supplementary variable to a MCA cloud of categories.}

\usage{
textvarsup(resmca, var, sel = 1:nlevels(var), axes = c(1, 2), 
           col = "black", app = 0, vname = NULL)
}

\arguments{
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}}
  \item{var}{the categorical supplementary variable. It does not need to have been used at the MCA step.}
  \item{sel}{numeric vector of indexes of the categories of the supplementary variable to be added to the plot (by default, labels are plotted for every categories)}
  \item{axes}{numeric vector of length 2, specifying the dimensions (axes) to plot (default is c(1,2))}
  \item{col}{color for the labels of the categories (default is black)}
  \item{app}{numerical value. If 0 (default), only the labels are plotted and their size is constant; if 1, only the labels are plotted and their size is proportional to the weights of the categories; if 2, points (triangles) and labels are plotted, and points size is proportional to the weight of the categories.}
  \item{vname}{a character string to be used as a prefix for the labels of the categories (null by default)}
}

\author{Nicolas Robette}

\seealso{
\code{\link{supvar}}, \code{\link{supvars}}, \code{\link{plot.speMCA}}, \code{\link{plot.csMCA}}
}

\examples{
# specific MCA of Music example data set
data(Music)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA")
mca <- speMCA(Music[,1:5], excl = junk)
# cloud of categories
# with Gender and Age supplementary variables
plot(mca, col = "gray")
textvarsup(mca, Music$Gender,col = "darkred")
textvarsup(mca, Music$Age, sel = c(1,3), col = "orange",
           vname = "age", app = 1)
}

\keyword{ aplot }
\keyword{ multivariate }
