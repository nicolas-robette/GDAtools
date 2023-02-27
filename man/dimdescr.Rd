\name{dimdescr}
\alias{dimdescr}

\title{Describes the dimensions of a MCA}

\description{
Identifies the variables and the categories that are the most characteristic according to each dimension obtained 
by a MCA. It is inspired by \code{\link{dimdesc}} function in \code{FactoMineR} package (see Husson et al, 2010), 
but allows to analyze variants of MCA, such as 'specific' MCA or 'class specific' MCA.
}

\usage{
dimdescr(resmca, vars = NULL, dim = c(1,2), 
         limit = NULL, correlation = "pearson",
         na.rm.cat = FALSE, na.value.cat = "NA", na.rm.cont = FALSE,
         nperm = NULL, distrib = "asympt")}

\arguments{
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}}
  \item{vars}{data frame of variables to describes the MCA dimensions with. If NULL (default), the active variables of the MCA will be used.}
  \item{dim}{the dimensions which are described. Default is c(1,2)}
  \item{limit}{for the relationship between a dimension and a categorical variable, only associations (point-biserial correlations) higher or equal to limit will be displayed. If NULL (default), they are all displayed.}
  \item{correlation}{character. The type of measure of correlation measure to use between two continuous variables : "pearson" (default), "spearman" or "kendall".}
  \item{na.rm.cat}{logical, indicating whether NA values in the categorical variables should be silently removed before the computation proceeds. If FALSE (default), an additional level is added to the categorical variables (see na.value.cat argument).}
  \item{na.value.cat}{character. Name of the level for NA category. Default is "NA". Only used if na.rm.cat = FALSE.}
  \item{na.rm.cont}{logical, indicating whether NA values in the continuous variables should be silently removed before the computation proceeds. Default is FALSE.}
  \item{nperm}{numeric. Number of permutations for the permutation test of independence. If NULL (default), no permutation test is performed.}
  \item{distrib}{the null distribution of permutation test of independence can be approximated by its asymptotic distribution (\code{"asympt"}, default) or via Monte Carlo resampling (\code{"approx"}).}
}

\details{
See \code{\link{condesc}}.
}

\value{
  Returns a list of ncp lists including: 
  \item{variables}{associations between y and the variables in x}
  \item{categories}{a data frame with categorical variables from x and associations measured by correlation coefficients}
}

\references{
Husson, F., Le, S. and Pages, J. (2010). \emph{Exploratory Multivariate Analysis by Example Using R}, Chapman and Hall.
}

\author{Nicolas Robette}

\seealso{
\code{\link{condesc}}, \code{\link{speMCA}}, \code{\link{csMCA}}, \code{\link{dimdesc}}
}

\examples{
## Performs a specific MCA on 'Music' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## and then describe the dimensions.
data(Music)
getindexcat(Music[,1:5])
mca <- speMCA(Music[,1:5],excl=c(3,6,9,12,15))
dimdescr(mca,limit=0.1,nperm=10)
}

\keyword{ multivariate }
\keyword{ misc }
