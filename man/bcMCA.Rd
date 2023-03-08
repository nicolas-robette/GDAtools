\name{bcMCA}
\alias{bcMCA}
\title{Between-class MCA}
\description{
Between-class MCA, also called Barycentric Discriminant Analysis
}
\usage{
bcMCA(data, class, row.w = NULL, ncp = 5)
}
\arguments{
  \item{data}{data frame with only categorical variables, i.e. factors}
  \item{class}{factor specifying the class}
  \item{row.w}{numeric vector of row weights. If NULL (default), a vector of 1 for uniform row weights is used.}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
}
\details{
Between-class MCA is sometimes also called Barycentric Discriminant Analysis or Discriminant Correspondence Analysis. It consists in three steps :
1. Transformation of \code{data} into an indicator matrix (i.e. disjunctive table)
2. Computation of the barycenter of the transformed data rows for each category of \code{class}
3. Correspondence Analysis of the set of barycenters
}
\value{
An object of class \code{CA} from \code{FactoMineR} package, with the indicator matrix as supplementary individuals, and an additional item :
\item{ratio}{the between-class inertia percentage}
}

\references{
Bry X., 1996, \emph{Analyses factorielles multiples}, Economica.

Lebart L., Morineau A. et Warwick K., 1984, \emph{Multivariate Descriptive Statistical Analysis}, John Wiley and sons, New-York.)
}

\author{Nicolas Robette}

\seealso{
\code{\link{PCAiv}}
}

\examples{
library(FactoMineR)
data(tea)
res <- bcMCA(tea[,1:18], tea$SPC)
plot(res, invisible = "row.sup")
}

\keyword{ multivariate }
