\name{pem}

\alias{pem}

\title{Computes the local and global Percentages of Maximum Deviation from Independence (PEM)}

\description{Computes the local and global Percentages of Maximum Deviation from Independence (PEM) of a contingency table.}

\usage{
pem(x,y,weights=rep(1,length(x)),digits=1,sort=TRUE)
}

\arguments{
  \item{x}{the first categorical variable}
  \item{y}{the second categorical variable}
  \item{weights}{an optional numeric vector of weights (by default, a vector of 1 for uniform weights)}
  \item{digits}{integer. The number of digits (default is 3).}
  \item{sort}{logical. Whether rows and columns are sorted according to a correspondence analysis or not (default is TRUE).}
}

\details{
The Percentage of Maximum Deviation from Independence (PEM) is an association measure for contingency tables and also provides attraction (resp. repulsion) measures in each cell of the crosstabulation (see Cibois, 1993). 
It is an alternative to khi2, Cramer's V coefficient, etc.
}

\value{
Returns a list:
  \item{peml }{Table with local percentages of maximum deviation from independence}
  \item{pemg }{Numeric value, i.e. the global percentage of maximum deviation from independence}
}

\references{
Cibois P., 1993, Le PEM, pourcentage de l'ecart maximum : un indice de liaison entre modalites d'un tableau de contingence, 
\emph{ Bulletin de methodologie sociologique}, n40, p.43-63. \url{https://cibois.pagesperso-orange.fr/bms93.pdf}
}

\author{Nicolas Robette}

\seealso{\code{\link{table}}, \code{\link{chisq.test}}, \code{\link{phi.table}}, \code{\link[vcd]{assocstats}}}

\examples{
## Computes the PEM for the contingency table
## of jazz and age variables
## from the 'Music' example data set
data(Music)
pem(Music$Jazz,Music$Age)
}

\keyword{ multivariate }
\keyword{ misc }
