\name{multiMCA}

\alias{multiMCA}

\title{Multiple Factor Analysis}

\description{
Performs Multiple Factor Analysis, drawing on the work of Escofier and Pages (1994). It allows the use of 
MCA variants (e.g. specific MCA or class specific MCA) as inputs.
}

\usage{multiMCA(l_mca, ncp = 5, compute.rv = FALSE)}

\arguments{
  \item{l_mca}{a list of objects of class \code{MCA}, \code{speMCA} or \code{csMCA}}
  \item{ncp}{number of dimensions kept in the results (default is 5)}
  \item{compute.rv}{whether RV coefficients should be computed or not (default is FALSE, which makes the function execute faster)}
}

\details{
This function binds individual coordinates from every MCA in \code{l_mca} argument, weights them by the first eigenvalue, and the resulting data frame is used as input for Principal Component Analysis (PCA).
}

\value{
  Returns an object of class \code{multiMCA}, i.e. a list:
  \item{eig}{a list of numeric vector for eigenvalues, percentage of variance and cumulative percentage of variance}
  \item{var}{a list of matrices with results for input MCAs components (coordinates, correlations between variables and axes, squared cosines, contributions)}
  \item{ind}{a list of matrices with results for individuals (coordinates, squared cosines, contributions)}
  \item{call}{a list with informations about input data}
  \item{VAR}{a list of matrices with results for categories and variables in the input MCAs (coordinates, squared cosines, test-values, variances)}
  \item{my.mca}{lists the content of the objects in \code{l_mca} argument}
  \item{RV}{a matrix of RV coefficients}
}

\references{
Escofier, B. and Pages, J. (1994) "Multiple Factor Analysis (AFMULT package)". \emph{Computational Statistics and Data Analysis}, 18, 121-140.
}

\author{Nicolas Robette}

\seealso{
\code{\link{plot.multiMCA}}, \code{\link{speMCA}}, \code{\link{csMCA}}
}

\examples{
data(Taste)
# specific MCA on music variables of Taste example data set
mca1 <- speMCA(Taste[,1:5], excl = c(3,6,9,12,15))
# specific MCA on movie variables of Taste example data set
mca2 <- speMCA(Taste[,6:11], excl = c(3,6,9,12,15,18))
# Multiple Factor Analysis of the two sets of variables
mfa <- multiMCA(list(mca1,mca2))
plot.multiMCA(mfa)
}

\keyword{ multivariate }
