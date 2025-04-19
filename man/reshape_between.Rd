\name{reshape_between}

\alias{reshape_between}

\title{Reshapes objects created with bcMCA()}

\description{reshapes objects created with `bcMCA()` so that they can be used with other functions from the package.}

\usage{reshape_between(bcmca)}

\arguments{
  \item{bcmca}{object created with \code{bcMCA} function}
}

\value{
  Returns an object of class \code{bcMCA}
}

\references{
Abdi H., 2007, "Discriminant Correspondence Analysis", In: Neil Salkind (Ed.), \emph{Encyclopedia of Measurement and Statistics}, Thousand Oaks (CA): Sage. 

Bry X., 1996, \emph{Analyses factorielles multiples}, Economica.

Lebart L., Morineau A. et Warwick K., 1984, \emph{Multivariate Descriptive Statistical Analysis}, John Wiley and sons, New-York.)
}

\author{Nicolas Robette}

\seealso{
\code{\link{bcMCA}}
}

\examples{
data(tea, package = "FactoMineR")
res <- bcMCA(tea[,1:18], tea$SPC)
res_ok <- reshape_between(res)
ggcloud_variables(res_ok)
}

\keyword{ multivariate }
\keyword{ misc }
