\name{MCAoiv}
\alias{MCAoiv}
\title{Multiple Correspondence Analysis with Orthogonal Instrumental Variables}

\description{
Multiple Correspondence Analysis with Orthogonal Instrumental Variables
}

\usage{
MCAoiv(X, Z, excl = NULL, row.w = NULL, ncp = 5)
}

\arguments{
  \item{X}{data frame with only factors}
  \item{Z}{data frame of instrumental variables, which can be numeric or factors. It must have the same number of rows as \code{X}.}
  \item{excl}{numeric vector indicating the indexes of the "junk" categories (default is NULL). See \code{\link{getindexcat}} or use \code{\link{ijunk}} interactive function to identify these indexes. It may also be a character vector of junk categories, specified in the form "namevariable.namecategory" (for instance "gender.male").} 
  \item{row.w}{Numeric vector of row weights. If NULL (default), a vector of 1 for uniform row weights is used.}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
}

\details{
Multiple Correspondence Analysis with Orthogonal Instrumental Variables consists in three steps :
1. Specific MCA of \code{Y}, keeping all the dimensions of the space
2. Computation of one linear regression for each dimension in the specific MCA, with individual coordinates as response and all variables in \code{X} as explanatory variables.
3. Principal Component Analysis of the set of residuals from the regressions in 2.
}

\value{
An object of class \code{PCA} from \code{FactoMineR} package, with \code{X} as supplementary variables, and an additional item :
\item{ratio}{the share of inertia not explained by the instrumental variables}.
}

\references{
Bry X., 1996, \emph{Analyses factorielles multiples}, Economica.

Lebart L., Morineau A. et Warwick K., 1984, \emph{Multivariate Descriptive Statistical Analysis}, John Wiley and sons, New-York.)
}

\author{Nicolas Robette}

\seealso{
\code{\link{wcMCA}}, \code{\link{wcPCA}}, \code{\link{PCAoiv}}
}

\examples{
library(FactoMineR)
data(tea)
mcaoiv <- MCAoiv(tea[,1:18], tea[,19:22])
mcaoiv$ratio
plot(mcaoiv, choix = "ind", invisible = "ind", col.quali = "black")
}

\keyword{ multivariate }
