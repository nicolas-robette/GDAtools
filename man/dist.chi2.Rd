\name{dist.chi2}
\alias{dist.chi2}

\title{Chi-squared distance}

\description{
Computes the chi-squared distance between the rows of a data frame of factors.
}

\usage{
dist.chi2(X)
}

\arguments{
  \item{X}{data frame. All variables should be factors.}
}

\details{
This function is adapted from \code{chi2Dist} function in \code{ExPosition} package.
}

\value{
A symmetrical matrix of distances
}

\author{Nicolas Robette}

\examples{
data(Music)
d <- dist.chi2(Music[,1:5])
# a short piece of the distance matrix
d[1:3, 1:3]
}

\keyword{ multivariate }
\keyword{ misc }
