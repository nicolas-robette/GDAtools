\name{wcPCA}
\alias{wcPCA}

\title{Within-class Principal Component Analysis}

\description{
Within-class Principal Component Analysis
}

\usage{
wcPCA(X, class, scale.unit = TRUE, ncp = 5, ind.sup = NULL, quanti.sup = NULL, 
          quali.sup = NULL, row.w = NULL, col.w = NULL, graph = FALSE, 
          axes = c(1, 2))
}

\arguments{
  \item{X}{a data frame with \emph{n} rows (individuals) and \emph{p} columns (numeric variables)}
  \item{class}{factor specifying the class}
  \item{scale.unit}{a boolean, if TRUE (default) then data are scaled to unit variance}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
  \item{ind.sup}{a vector indicating the indexes of the supplementary individuals}
  \item{quanti.sup}{a vector indicating the indexes of the quantitative supplementary variables}
  \item{quali.sup}{a vector indicating the indexes of the categorical supplementary variables}
  \item{row.w}{an optional row weights (by default, a vector of 1 for uniform row weights); the weights are given only for the active individuals}
  \item{col.w}{an optional column weights (by default, uniform column weights); the weights are given only for the active variables}
  \item{graph}{boolean, if TRUE a graph is displayed. Default is FALSE.}
  \item{axes}{a length 2 vector specifying the components to plot}
}

\details{
Within-class Principal Component Analysis is a PCA where the active variables are centered on the mean of their class instead of the overall mean.

It is a "conditional" PCA and can be seen as a special case of PCA with orthogonal instrumental variables, with only one (categorical) instrumental variable.
}

\value{
An object of class \code{PCA} from \code{FactoMineR} package, with an additional item :
\item{ratio}{the within-class inertia percentage}.
}

\note{
The code is adapted from \code{PCA} function from \code{FactoMineR} package.
}

\references{
Escofier B., 1990, Analyse des correspondances multiples conditionnelle, \emph{La revue de Modulad}, 5, 13-28.

Lebart L., Morineau A. et Warwick K., 1984, \emph{Multivariate Descriptive Statistical Analysis}, John Wiley and sons, New-York.)
}

\author{Nicolas Robette}

\seealso{
\code{\link{PCAoiv}}, \code{\link{wcMCA}}, \code{\link{MCAoiv}}
}

\examples{
# within-class analysis of decathlon data
# with quatiles of points as class
library(FactoMineR)
data(decathlon)
points <- cut(decathlon$Points, c(7300, 7800, 8000, 8120, 8900), c("Q1","Q2","Q3","Q4"))
res <- wcPCA(decathlon[,1:10], points)
plot(res, choix = "var")
}

\keyword{ multivariate }
