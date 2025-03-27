\name{bcMCA}
\alias{bcMCA}
\title{Between-class MCA}
\description{
Between-class MCA, also called Barycentric Discriminant Analysis
}
\usage{
bcMCA(data, class, excl = NULL, row.w = NULL)
}
\arguments{
  \item{data}{data frame with only categorical variables, i.e. factors}
  \item{class}{factor specifying the class}
  \item{excl}{numeric vector indicating the indexes of the "junk" categories (default is NULL). See \code{\link{getindexcat}} or use \code{\link{ijunk}} interactive function to identify these indexes. It may also be a character vector of junk categories, specified in the form "namevariable.namecategory" (for instance "gender.male").}
  \item{row.w}{numeric vector of row weights. If NULL (default), a vector of 1 for uniform row weights is used.}
}
\details{
Between-class MCA is sometimes also called Barycentric Discriminant Analysis or Discriminant Correspondence Analysis. It consists in three steps :
1. Transformation of \code{data} into an indicator matrix (i.e. disjunctive table)
2. Computation of the barycenter of the transformed data for each category of \code{class}
3. Correspondence Analysis of the set of barycenters
Between-class MCA can also be viewed as a special case of MCA with instrumental variables, with only one categorical instrumental variable.
}

\value{
An object of class \code{CA} from \code{FactoMineR} package, with the indicator matrix of \code{data} as supplementary rows, and an additional item :
\item{ratio}{the between-class inertia percentage}
}

\references{
Abdi H., 2007, "Discriminant Correspondence Analysis", In: Neil Salkind (Ed.), \emph{Encyclopedia of Measurement and Statistics}, Thousand Oaks (CA): Sage. 

Bry X., 1996, \emph{Analyses factorielles multiples}, Economica.

Lebart L., Morineau A. et Warwick K., 1984, \emph{Multivariate Descriptive Statistical Analysis}, John Wiley and sons, New-York.)
}

\author{Nicolas Robette}

\seealso{
\code{\link{DAQ}}, \code{\link{MCAiv}}
}

\examples{
library(FactoMineR)
data(tea)
res <- bcMCA(tea[,1:18], tea$SPC)
# categories of class
plot(res, invisible = c("col", "row.sup"))
# Variables in tea data
plot(res, invisible = c("row", "row.sup"))
# between-class inertia percentage
res$ratio
}

\keyword{ multivariate }
