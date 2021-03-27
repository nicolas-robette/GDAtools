\name{wtable}
\alias{wtable}

\title{Computes a (possibly weighted) contingency table}
\description{Computes a contingency table from one or two vectors, with the possibility of specifying weights.}

\usage{wtable(x,y=NULL,weights=rep.int(1,length(x)),stat="freq",
              digits=1,mar=TRUE,na_value=NULL)}
\arguments{
  \item{x}{an object which can be interpreted as factor}
  \item{y}{an optional object which can be interpreted as factor}
  \item{weights}{an optional numeric vector of weights (by default, a vector of 1 for uniform weights)}
  \item{stat}{character. Whether to compute a contingenct table ("freq", default, percentages ("prop"), row percentages ("rprop") or column percentages ("cprop").}
  \item{digits}{integer indicating the number of decimal places (default is 1)}
  \item{mar}{logical. If TRUE (default), margins are computed}
  \item{na_value}{character. Name of the level for NA category. If NULL (default), NA values are ignored.}
}


\value{Returns a contingency table.}

\author{Nicolas Robette}

\seealso{\code{\link{table}}, \code{\link{assoc.twocat}}}

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
