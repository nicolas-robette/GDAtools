\name{coiPCA}
\alias{coiPCA}

\title{Coinertia analysis between two groups of numerical variables}

\description{
Coinertia analysis between two groups of numerical variables
}

\usage{
coiPCA(Xa, Xb, row.w = NULL, ncp = 5)
}

\arguments{
  \item{Xa}{data frame with the first group of numerical variables}
  \item{Xb}{data frame with the second group of numerical variables}
  \item{row.w}{numeric vector of row weights. If NULL (default), a vector of 1 for uniform row weights is used.}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
}

\details{
Coinertia analysis aims at capturing the structure common to two groups of variables. With groups of numerical variables, it is equivalent to Tucker's inter-battery analysis.
It consists in the following steps :
1. Variables in Xa and Xb are centered and scaled
2. Computation of the covariance matrix t(Xa).Xb
3. PCA of the matrix
}

\value{
An object of class \code{PCA} from \code{FactoMineR} package, with an additional item :
\item{RV}{the RV coefficient between the two groups of variabels}
}

\references{
Tucker, L.R. (1958) An inter-battery method of factor analysis. \emph{Psychometrika}, 23-2, 111-136.

Dolédec, S. and Chessel, D. (1994) Co-inertia analysis: an alternative method for studying species-environment relationships. \emph{Freshwater Biology}, 31, 277–294.
}

\author{Nicolas Robette}

\seealso{
\code{\link{coiMCA}}, \code{\link{rvcoef}}
}

\examples{
library(FactoMineR)
data(decathlon)
# variables of results for each sport 
Xa <- decathlon[,1:10]
# rank and points variables
Xb <- decathlon[,11:12]
# coinertia analysis
res <- coiPCA(Xa, Xb)
# plot of variables in Xa
plot(res, choix = "ind")
# plot of variables in Xb
plot(res, choix = "var")
# RV coefficient
res$RV
}

\keyword{ multivariate }
