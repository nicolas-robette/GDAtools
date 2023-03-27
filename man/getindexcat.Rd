\name{getindexcat}

\alias{getindexcat}

\title{Names of the categories in a data frame}

\description{Returns a vector of names corresponding the the categories in a data frame exclusively composed of categorical variables.}

\usage{getindexcat(data)}

\arguments{
  \item{data}{data frame of categorical variables}
}

\details{This function may be useful prior to a specific MCA, to identify the indexes of the 'junk' categories to exclude.}

\value{Returns a character vector with the names of the categories of the variables in the data frame}

\author{Nicolas Robette}

\seealso{
\code{\link{ijunk}}, \code{\link{speMCA}}, \code{\link{csMCA}}
}

\examples{
data(Music)
getindexcat(Music[,1:5])
mca <- speMCA(Music[,1:5], excl = c(3,6,9,12,15))
}

\keyword{ multivariate }
\keyword{ misc }
