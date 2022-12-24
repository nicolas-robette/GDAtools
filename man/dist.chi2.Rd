\name{dist.chi2}
\alias{dist.chi2}

\title{Computes the chi-squared distance}

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

\author{Nicolas Robette}

\examples{
data(Music)
d <- dist.chi2(Music[,1:5])
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ misc }
