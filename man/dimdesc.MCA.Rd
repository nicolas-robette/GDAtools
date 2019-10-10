\name{dimdesc.MCA}
\alias{dimdesc.MCA}

\title{Describes the dimensions of MCA and variants of MCA}

\description{
Identifies the variables and the categories that are the most characteristic according to each dimension obtained 
by a Factor Analysis. It is inspired by \code{\link{dimdesc}} function in \code{FactoMineR} package (see Husson et al, 2010), 
but allows to analyze variants of MCA, such as 'specific' MCA or 'class specific' MCA.
}

\usage{dimdesc.MCA(resmca, ncp = 3, proba = 0.05)}

\arguments{
  \item{resmca}{object of class \code{'MCA'}, \code{'speMCA'}, \code{'csMCA'} or \code{'stMCA'}}
  \item{ncp}{number of dimensions to describe (default is 3)}
  \item{proba}{the significance threshold considered to characterize the dimension (default is 0.05)}
}

\details{
The statistical indicator used for variables is square correlation ratio (R2) 
and the one used for categories is test-value (v.test).
}

\value{
  Returns a list of ncp lists including: 
  \item{quali}{the description of the dimensions by the categorical variables (the variables are sorted)}
  \item{category}{the description of the dimensions by each category of all the categorical variables (the categories are sorted)}
}

\references{
Husson, F., Le, S. and Pages, J. (2010). \emph{Exploratory Multivariate Analysis by Example Using R}, Chapman and Hall.
}

\author{Nicolas Robette}

\seealso{
\code{\link{dimdesc}}, \code{\link{condes}}, \code{\link{speMCA}}, \code{\link{csMCA}}
}

\examples{
## Performs a specific MCA on 'Music' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## and then describe the dimensions.
data(Music)
getindexcat(Music[,1:5])
mca <- speMCA(Music[,1:5],excl=c(3,6,9,12,15))
dimdesc.MCA(mca,proba=0.2)
}

\keyword{ multivariate }
\keyword{ misc }
