\name{rvcoef}
\alias{rvcoef}

\title{RV coefficient}

\description{
Computes the RV coefficient between two groups of numerical variables.
}

\usage{
rvcoef(Xa, Xb, row.w = NULL)
}

\arguments{
  \item{Xa}{data frame with the first group of numerical variables}
  \item{Xb}{data frame with the second group of numerical variables}
  \item{row.w}{numeric vector of row weights. If NULL (default), a vector of 1 for uniform row weights is used.}
}

\details{
\code{Xa} and \code{Xb} should have the same number of rows.
}

\value{
numerical value : the RV coefficient
}

\references{
Escouffier, Y. (1973) Le traitement des variables vectorielles. \emph{Biometrics} 29 751–760.
}

\author{
Nicolas Robette
}

\seealso{
\code{\link{coiPCA}}, \code{\link{coiMCA}}, \code{\link{multiMCA}}
}

\examples{
# RV coefficient between decathlon results by sport
# and Rank and Points
library(FactoMineR)
data(decathlon)
Xa <- decathlon[,1:10]
Xb <- decathlon[,11:12]
str(Xa)
str(Xb)
rvcoef(Xa, Xb)
}

\keyword{ multivariate }
\keyword{ misc }
