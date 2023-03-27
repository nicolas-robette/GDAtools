\name{PCAoiv}
\alias{PCAoiv}
\title{Principal Component Analysis with Orthogonal Instrumental Variables}

\description{
Principal Component Analysis with Orthogonal Instrumental Variables
}

\usage{
PCAoiv(X, Z, row.w = NULL, ncp = 5)
}

\arguments{
  \item{X}{data frame with only numeric variables}
  \item{Z}{data frame of instrumental variables to be "partialled out"", which can be numeric or factors. It must have the same number of rows as \code{X}.}
  \item{row.w}{Numeric vector of row weights. If NULL (default), a vector of 1 for uniform row weights is used.}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
}

\details{
Principal Component Analysis with Orthogonal Instrumental Variables consists in two steps :
1. Computation of one linear regression for each variable in \code{X}, with this variable as response and all variables in \code{Z} as explanatory variables.
2. Principal Component Analysis of the set of residuals from the regressions in 1.
}

\value{
An object of class \code{PCA} from \code{FactoMineR} package, and an additional item :
\item{ratio}{the share of inertia not explained by the instrumental variables}.
}

\references{
Bry X., 1996, \emph{Analyses factorielles multiples}, Economica.

Lebart L., Morineau A. et Warwick K., 1984, \emph{Multivariate Descriptive Statistical Analysis}, John Wiley and sons, New-York.)
}

\author{Nicolas Robette}

\seealso{
\code{\link{wcPCA}}, \code{\link{wcMCA}}, \code{\link{MCAoiv}}
}

\examples{
library(FactoMineR)
data(decathlon)
pcaoiv <- PCAoiv(decathlon[,1:10], decathlon[,12:13])
plot(pcaoiv, choix = "var", invisible = "quanti.sup")
}

\keyword{ multivariate }
