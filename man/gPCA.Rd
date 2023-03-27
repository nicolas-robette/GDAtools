\name{gPCA}
\alias{gPCA}

\title{Generalized Principal Component Analysis}

\description{
Generalized Principal Component Analysis
}

\usage{
gPCA(X, row.w = NULL, col.w = NULL, center = FALSE, scale = FALSE, tol = 1e-07)
}

\arguments{
  \item{X}{data frame of active variables}
  \item{row.w}{numeric vector of row weights. If NULL (default), a vector of 1 for uniform row weights is used.}
  \item{col.w}{numeric vector of column weights. If NULL (default), a vector of 1 for uniform column weights is used.}
  \item{center}{logical. If TRUE, variables are centered (default is FALSE).}  
  \item{scale}{logical. If TRUE, variables are scaled to unit variance (default is FALSE).} 
  \item{tol}{a tolerance threshold for null eigenvalues (a value less than \code{tol} times the first one is considered as null)}
}

\details{
Generalized PCA is basically a PCA with the possibility to specify row weights (i.e. "masses") and variable weights (i.e. the "metric"), and to choose whether to center and scale the variables. This flexibility makes it the building block of many variants of PCA, such as Correspondence Analysis and Multiple Correspondence Analysis.

Generalized PCA is also known as "biweighted PCA", "duality diagram" or "generalized singular value decomposition".
}

\value{
An object of class \code{PCA} from \code{FactoMineR} package
}

\references{
Bry X., 1995, \emph{Analyses factorielles simples}, Economica.

Escofier B. and Pagès J., \emph{Analyses factorielles simples et multiples}, Dunod (2008).

Escoufier, Y. (1987) The duality diagram : a means of better practical applications In \emph{Development in numerical ecology}, Legendre, P. & Legendre, L. (Eds.) NATO advanced Institute, Serie G. Springer Verlag, Berlin, 139–156.
}

\author{Nicolas Robette}

\examples{
library(FactoMineR)
data(decathlon)
res <- gPCA(decathlon[,1:10], center = TRUE, scale = TRUE)
plot(res, choix = "var")
}

\keyword{ multivariate }
