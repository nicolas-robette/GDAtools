\name{wcMCA}
\alias{wcMCA}
\title{Within-class MCA}

\description{
Within-class MCA, also called conditional MCA
}

\usage{
wcMCA(data, class, excl = NULL, row.w = NULL, ncp = 5)
}

\arguments{
  \item{data}{data frame with only categorical variables, i.e. factors}
  \item{class}{factor specifying the class}
  \item{excl}{numeric vector indicating the indexes of the "junk" categories (default is NULL). See \code{\link{getindexcat}} or use \code{\link{ijunk}} interactive function to identify these indexes. It may also be a character vector of junk categories, specified in the form "namevariable.namecategory" (for instance "gender.male").}
  \item{row.w}{numeric vector of row weights. If NULL (default), a vector of 1 for uniform row weights is used.}
  \item{ncp}{number of dimensions kept in the results (by default 5)}
}

\details{
Within-class Multiple Correspondence Analysis is a MCA where the active categories are centered on the mean of their class (i.e. conditional frequencies) instead of the overall mean (i.e. marginal frequencies).

It is also known as "conditional MCA" and can be seen as a special case of MCA on orthogonal instrumental variables, with only one (categorical) instrumental variable.
}

\value{
An object of class \code{speMCA}, with an additional item :
\item{ratio}{the within-class inertia percentage}.
}

\note{
The code is adapted from \code{speMCA} function.

As in \code{speMCA}, if there are NAs in \code{data}, these NAs will be automatically considered as junk categories. If one desires more flexibility, \code{data} should be recoded to add explicit factor levels for NAs and then \code{excl} option may be used to select the junk categories.
}

\references{
Escofier B., 1990, Analyse des correspondances multiples conditionnelle, \emph{La revue de Modulad}, 5, 13-28.

Lebart L., Morineau A. et Warwick K., 1984, \emph{Multivariate Descriptive Statistical Analysis}, John Wiley and sons, New-York.)
}

\author{Nicolas Robette}

\seealso{
\code{\link{MCAoiv}}, \code{\link{wcPCA}}, \code{\link{PCAoiv}}
}

\examples{
# within-class analysis of tea data
# with SPC as class
library(FactoMineR)
data(tea)
res <- wcMCA(tea[,1:18], tea$SPC)
res$ratio
ggcloud_variables(res)
}

\keyword{ multivariate }
