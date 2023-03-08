\name{PCAiv}
\alias{PCAiv}
\title{Principal Component Analysis with Instrumental Variables}
\description{
Principal Component Analysis with Instrumental Variables
}
\usage{
PCAiv(Y, X, row.w = NULL, ncp = 5)
}
\arguments{
  \item{Y}{data frame with only numeric variables}
  \item{X}{data frame of instrumental variables, which can be numeric or factors. It must have the same number of rows as \code{Y}.}
  \item{row.w}{Numeric vector of row weights. If NULL (default), a vector of 1 for uniform row weights is used.}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
}
\details{
Principal Component Analysis with Instrumental Variables consists in two steps :
1. Computation of one linear regression for each variable in \code{Y}, with this variable as response and all variables in \code{X} as explanatory variables.
2. Principal Component Analysis of the set of predicted values from the regressions in 1 ("Y hat").
}
\value{
An object of class \code{PCA} from \code{FactoMineR} package, with \code{X} as supplementary variables, and an additional item :
\item{ratio}{the share of inertia explained by the instrumental variables}.
}

\references{
Bry X., 1996, \emph{Analyses factorielles multiples}, Economica.

Lebart L., Morineau A. et Warwick K., 1984, \emph{Multivariate Descriptive Statistical Analysis}, John Wiley and sons, New-York.)
}

\author{Nicolas Robette}

\seealso{
\code{\link{bcMCA}}, \code{\link{PCAoiv}}
}

\examples{
library(FactoMineR)
data(decathlon)
pcaiv <- PCAiv(decathlon[,1:10], decathlon[,12:13])
plot(pcaiv, choix = "var")
plot(pcaiv, choix = "ind", invisible = "ind")
}

\keyword{ multivariate }
