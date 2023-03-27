\name{coiMCA}
\alias{coiMCA}

\title{Coinertia analysis between two groups of categorical variables}

\description{
Coinertia analysis between two groups of categorical variables
}

\usage{
coiMCA(Xa, Xb, 
       excl.a = NULL, excl.b = NULL,
       row.w = NULL, ncp = 5)
}

\arguments{
  \item{Xa}{data frame with the first group of categorical variables}
  \item{Xb}{data frame with the second group of categorical variables}
  \item{excl.a}{numeric vector indicating the indexes of the "junk" categories in \code{Xa} (default is NULL). See \code{\link{getindexcat}} or use \code{\link{ijunk}} interactive function to identify these indexes. It may also be a character vector of junk categories, specified in the form "namevariable.namecategory" (for instance "gender.male").}
  \item{excl.b}{numeric vector indicating the indexes of the "junk" categories in \code{Xb} (default is NULL). See \code{excl.a} argument.}
  \item{row.w}{numeric vector of row weights. If NULL (default), a vector of 1 for uniform row weights is used.}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
}

\details{
Coinertia analysis aims at capturing the structure common to two groups of variables. With groups of numerical variables, it is equivalent to Tucker's inter-battery analysis.
With categorical data, it consists in the following steps :
1. Transformation of \code{Xa} and \code{Xb} into indicator matrices (i.e. disjunctive tables) \code{Xad} and \code{Xbd}
2. Computation of the covariance matrix t(Xad).Xbd
3. CA of the matrix
}

\value{
An object of class \code{CA} from \code{FactoMineR} package, with an additional item :
\item{RV}{the RV coefficient between the two groups of variabels}
}

\references{
Tucker, L.R.. (1958) An inter-battery method of factor analysis. \emph{Psychometrika}, 23-2, 111-136.

Dolédec, S. and Chessel, D. (1994) Co-inertia analysis: an alternative method for studying species-environment relationships. \emph{Freshwater Biology}, 31, 277–294.
}

\author{Nicolas Robette}

\seealso{
\code{\link{coiPCA}}, \code{\link{rvcoef}}
}

\examples{
data(Music)
# music tastes
Xa <- Music[,1:5]
# gender and age
Xb <- Music[,6:7]
# coinertia analysis
res <- coiMCA(Xa, Xb)
plot(res)
# RV coefficient
res$RV
}

\keyword{ multivariate }
