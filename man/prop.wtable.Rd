\name{prop.wtable}
\alias{prop.wtable}

\title{Transforms a (possibly weighted) contingency table into percentages}
\description{Computes a contingency table from one or two vectors, with the possibility of specifying weights, and then computes the percentages.}

\usage{prop.wtable(var1,var2=NULL,w=rep.int(1,length(var1)),dir=0,digits=1,mar=TRUE,na=TRUE)}

\arguments{
  \item{var1}{an object which can be interpreted as factor}
  \item{var2}{an optional object which can be interpreted as factor}
  \item{w}{an optional numeric vector of weights (by default, a vector of 1 for uniform weights)}
  \item{dir}{integer: 0 for percentages, 1 for row percentages and 2 for column percentages (default is 0)}
  \item{digits}{integer indicating the number of decimal places (default is 1)}
  \item{mar}{logical. If TRUE (default), margins are computed}
  \item{na}{logical. If TRUE (default), 'NA' are treated as a category. If FALSE, they are ignored}
}

\value{Returns a contingency table expressed in percentages in matrix format.}

\author{Nicolas Robette}

\seealso{\code{\link{wtable}}, \code{\link{prop.table}}}

\examples{
## Computes a contingency table
## (expressed in percentages)
## of jazz and age variables
## from the 'Music' example data set
## with or without weights
data(Music)
prop.wtable(Music$Jazz)
prop.wtable(Music$Jazz,Music$Gender)
prop.wtable(Music$Jazz,Music$Gender,dir=1)
prop.wtable(Music$Jazz,Music$Gender,dir=2)

weight <- sample(0:20,nrow(Music),TRUE)/10
prop.wtable(Music$Jazz,w=weight)
prop.wtable(Music$Jazz,Music$Age,weight)
prop.wtable(Music$Jazz,Music$Age,weight,dir=1)
prop.wtable(Music$Jazz,Music$Age,weight,dir=2)
}

\keyword{ multivariate }
\keyword{ misc }
