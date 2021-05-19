\name{condesc}
\alias{condesc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Measures the association between a continuous variable and some continuous and/or categorical variables}
\description{
Measures the association between a continuous variable and some continuous and/or categorical variables}
\usage{
condesc(y, x, weights=rep(1,length(y)), min.cor=NULL, 
robust=TRUE, nperm=NULL, distrib="asympt", dec=c(3,3,0,3))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{the continuous variable to describe}
  \item{x}{a data frame with continuous and/or categorical variables}
  \item{weights}{an optional numeric vector of weights (by default, a vector of 1 for uniform weights)}
  \item{min.cor}{for the relationship between y and a categorical variable, only associations higher or equal to min.cor will be displayed. If NULL (default), they are all displayed.}
  \item{robust}{logical. If FALSE, mean and standard deviation are used instead of median and mad. Default is TRUE.}
  \item{nperm}{numeric. Number of permutations for the permutation test of independence. If NULL (default), no permutation test is performed.}
  \item{distrib}{the null distribution of permutation test of independence can be approximated by its asymptotic distribution (\code{"asympt"}, default) or via Monte Carlo resampling (\code{"approx"}).}
  \item{dec}{vector of 4 integers for number of decimals. The first value if for association measures, the second for permutation p-values, the third for medians and mads, the fourth for point biserial correlations. Default is c(3,3,0,3).}
}

\value{
A list of the following items :
\item{variables}{associations between y and the variables in x}
\item{categories}{a data frame with categorical variables from x and associations measured by point biserial correlation}
}
\references{
Rakotomalala R., 'Comprendre la taille d'effet (effect size)', [http://eric.univ-lyon2.fr/~ricco/cours/slides/effect_size.pdf]
}
\author{Nicolas Robette}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[FactoMineR]{condes}}, \code{\link{catdesc}}, \code{\link{assoc.yx}}, \code{\link{darma}}

}
\examples{
data(Movies)
condesc(Movies$BoxOffice, Movies[,c("Budget","Genre","Country")])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ misc }
