\name{catdesc}
\alias{catdesc}

\title{Measures the association between a categorical variable and some continuous and/or categorical variables}
\description{
Measures the association between a categorical variable and some continuous and/or categorical variables}
\usage{
catdesc(y, x, weights=rep(1,length(y)), min.phi=NULL, 
robust=TRUE, nperm=NULL, distrib="asympt", dec=c(3,3,3,3,1,3))
}

\arguments{
  \item{y}{the categorical variable to describe (must be a factor)}
  \item{x}{a data frame with continuous and/or categorical variables}
  \item{weights}{an optional numeric vector of weights (by default, a vector of 1 for uniform weights)}
  \item{min.phi}{for the relationship between y and a categorical variable, only associations higher or equal to min.phi will be displayed. If NULL (default), they are all displayed.}
  \item{robust}{logical. If FALSE, mean and standard deviation are used instead of median and mad. Default is TRUE.}
  \item{nperm}{numeric. Number of permutations for the permutation test of independence. If NULL (default), no permutation test is performed.}
  \item{distrib}{the null distribution of permutation test of independence can be approximated by its asymptotic distribution (\code{"asympt"}, default) or via Monte Carlo resampling (\code{"approx"}).}
  \item{dec}{vector of 6 integers for number of decimals. The first value if for association measures, the second for permutation p-values, the third for percents, the fourth for phi coefficients, the fifth for medians and mads, the sixth for point biserial correlations. Default is c(3,3,3,3,1,3).}
}

\value{
A list of the following items :
\item{variables}{associations between y and the variables in x}
\item{bylevel}{a list with one element for each level of y}
Each element in bylevel has the following items :
\item{categories}{a data frame with categorical variables from x and associations measured by phi}
\item{continuous.var}{a data frame with continuous variables from x and associations measured by correlation coefficients}
}
\references{
Rakotomalala R., 'Comprendre la taille d'effet (effect size)', [http://eric.univ-lyon2.fr/~ricco/cours/slides/effect_size.pdf]
}
\author{Nicolas Robette}

\seealso{
\code{\link[FactoMineR]{catdes}}, \code{\link{condesc}}, \code{\link{assoc.yx}}, \code{\link{darma}}
}
\examples{
data(Movies)
catdesc(Movies$ArtHouse, Movies[,c("Budget","Genre","Country")])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ misc }
