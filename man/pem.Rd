\name{pem}

\alias{pem}

\title{Computes the local and global Percentages of Maximum Deviation from Independance (PEM)}

\description{Computes the local and global Percentages of Maximum Deviation from Independance (PEM) of a contingency table.}

\usage{pem(x)}

\arguments{
  \item{x}{Contingency table. Accepted formats are matrices and 'table' objects.}
}

\details{
The Percentage of Maximum Deviation from Independance (PEM) is an association measure for contingency tables (see Cibois, 1993). 
It is an alternative to khi2, Cramer coefficient, etc.
}

\value{
Returns a list:
  \item{peml }{Matrix with local percentages of maximum deviation from independance}
  \item{pemg }{Numeric value, i.e. the global percentage of maximum deviation from independance}
}

\references{
Cibois P., 1993, Le PEM, pourcentage de l'ecart maximum : un indice de liaison entre modalites d'un tableau de contingence, 
\emph{ Bulletin de methodologie sociologique}, n40, p.43-63. \url{http://cibois.pagesperso-orange.fr/bms93.pdf}
}

\author{Nicolas Robette}

\seealso{\code{\link{table}}, \code{\link{chisq.test}}, \code{\link[vcd]{assocstats}}}

\examples{
## Computes the PEM for the contingency table
## of jazz and age variables
## from the 'Music' example data set
data(Music)
x <- table(Music$Jazz,Music$Age)
pem(x)
}

\keyword{ multivariate }
\keyword{ misc }
