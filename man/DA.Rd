\name{DA}
\alias{DA}

\title{Discriminant Analysis}

\description{Descriptive discriminant analysis, aka "Analyse Factorielle Discriminante" for the French school of multivariate data analysis.}

\usage{
DA(data, class, row.w = NULL, type = "FR")
}

\arguments{
  \item{data}{data frame with only numeric variables}
  \item{class}{factor specifying the class}
  \item{row.w}{numeric vector of row weights. If NULL (default), a vector of 1 for uniform row weights is used.}
  \item{type}{If "FR" (default), the inverse of the total covariance matrix is used as metric. If "GB", it is the inverse of the within-class covariance matrix (Mahalanobis metric), which makes the results equivalent to linear discriminant analysis as implemented in \code{lda} function in \code{MASS} package.}  
}

\details{
The results are the same with \code{type} "FR" or "GB", only the eigenvalues vary. With \code{type="FR"}, these eigenvalues vary between 0 and 1 and can be interpreted as "discriminant power".
}

\value{
An object of class \code{PCA} from \code{FactoMineR} package, with \code{class} as qualitative supplementary variable, and one additional item :
\item{cor_ratio}{correlation ratios between \code{class} and the discriminant factors}
}

\references{
Bry X., 1996, \emph{Analyses factorielles multiples}, Economica.

Lebart L., Morineau A. et Warwick K., 1984, \emph{Multivariate Descriptive Statistical Analysis}, John Wiley and sons, New-York.)

Saporta G., 2006, \emph{Probabilités, analyses des données et statistique}, Editions Technip.
}

\author{Marie Chavent, Nicolas Robette}

\note{
The code is adapted from a script from Marie Chavent.
See: https://marie-chavent.perso.math.cnrs.fr/teaching/
}

\seealso{
\code{\link{bcPCA}}, \code{\link{PCAiv}}
}

\examples{
library(FactoMineR)
data(decathlon)
points <- cut(decathlon$Points, c(7300, 7800, 8000, 8120, 8900), c("Q1","Q2","Q3","Q4"))
res <- DA(decathlon[,1:10], points)
# plot of observations colored by class
plot(res, choix = "ind", invisible = "quali", habillage = res$call$quali.sup$numero)
# plot of class categories
plot(res, choix = "ind", invisible = "ind", col.quali = "darkblue")
# plot of variables
plot(res, choix = "varcor", invisible = "none")
}

\keyword{ multivariate }
