\name{wtable}
\alias{wtable}

\title{Computes a (possibly weighted) contingency table}
\description{Computes a contingency table from one or two vectors, with the possibility of specifying weights.}

\usage{wtable(var1,var2=NULL,w=rep.int(1,length(var1)),digits=0,mar=TRUE,na=TRUE)}
\arguments{
  \item{var1}{an object which can be interpreted as factor}
  \item{var2}{an optional object which can be interpreted as factor}
  \item{w}{an optional numeric vector of weights (by default, a vector of 1 for uniform weights)}
  \item{digits}{integer indicating the number of decimal places (default is 0)}
  \item{mar}{logical. If TRUE (default), margins are computed}
  \item{na}{logical. If TRUE (default), 'NA' are treated as a category. If FALSE, they are ignored}
}


\value{Returns a contingency table in matrix format.}

\author{Nicolas Robette}

\seealso{\code{\link{table}}, \code{\link{prop.wtable}}}

\examples{
## Computes a contingency table
## of jazz and age variables
## from the 'Music' example data set
## with or without weights
data(Music)
wtable(Music$Jazz)
wtable(Music$Jazz,Music$Age)

weight <- rep(c(0,0.5,1,1.5,2), length.out=nrow(Music))
wtable(Music$Jazz,w=weight,digits=1)
wtable(Music$Jazz,Music$Age,weight,1)
}

\keyword{ multivariate }
\keyword{ misc }
