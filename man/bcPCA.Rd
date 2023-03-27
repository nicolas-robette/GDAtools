\name{bcPCA}
\alias{bcPCA}

\title{Between-class Principal Component Analysis}

\description{
Between-class Principal Component Analysis
}

\usage{
bcPCA(data, class, row.w = NULL, scale.unit = TRUE, ncp = 5)
}

\arguments{
  \item{data}{data frame with only numeric variables}
  \item{class}{factor specifying the class}
  \item{row.w}{numeric vector of row weights. If NULL (default), a vector of 1 for uniform row weights is used.}
  \item{scale.unit}{logical. If TRUE (default) then data are scaled to unit variance.}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
}

\details{
Between-class Principal Component Analysis consists in two steps :
1. Computation of the barycenter of \code{data} rows for each category of \code{class}
2. Principal Component Analysis of the set of barycenters

It is a quite similar to Linear Discriminant Analysis, but the metric is different.

It can be seen as a special case of PCA with instrumental variables, with only one categorical instrumental variable.
}

\value{
An object of class \code{PCA} from \code{FactoMineR} package, with the original data as supplementary individuals, and an additional item :
\item{ratio}{the between-class inertia percentage}
}

\references{
Bry X., 1996, \emph{Analyses factorielles multiples}, Economica.

Lebart L., Morineau A. et Warwick K., 1984, \emph{Multivariate Descriptive Statistical Analysis}, John Wiley and sons, New-York.)
}

\author{Nicolas Robette}

\seealso{
\code{\link{PCAiv}}, \code{\link{DA}}
}

\examples{
library(FactoMineR)
data(decathlon)
points <- cut(decathlon$Points, c(7300, 7800, 8000, 8120, 8900), c("Q1","Q2","Q3","Q4"))
res <- bcPCA(decathlon[,1:10], points)
# categories of class
plot(res, choix = "ind", invisible = "ind.sup")
# variables in decathlon data
plot(res, choix = "var")
# between-class inertia percentage
res$ratio
}

\keyword{ multivariate }
