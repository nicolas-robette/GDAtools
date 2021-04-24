\name{darma}
\alias{darma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Describes Associations as in a Regression Model Analysis.
}
\description{
  Computes bivariate association measures between a response and predictor variables, producing a summary looking like a regression analysis.
}
\usage{
  darma(y, x, weights=rep(1,length(y)), target=1, twocont="kendall",
          nperm=NULL, distrib="asympt", dec=c(1,3,3))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{the response variable}
  \item{x}{the predictor variables}
  \item{weights}{an optional numeric vector of weights (by default, a vector of 1 for uniform weights)}
  \item{target}{rank or name of the category of interest when y is categorical}
  \item{twocont}{character. The type of measure of correlation measure to use between two continuous variables : "pearson", "spearman" or "kendall" (default).}
  \item{nperm}{numeric. Number of permutations for the permutation test of independence. If NULL (default), no permutation test is performed.}
  \item{distrib}{the null distribution of permutation test of independence can be approximated by its asymptotic distribution (\code{"asympt"}, default) or via Monte Carlo resampling (\code{"approx"}).}
  \item{dec}{vector of 3 integers for number of decimals. The first value if for percents or medians, the second for association measures, the third for permutation p-values. Default is c(1,3,3).}
}

\details{
The function computes association measures (phi, correlation coefficient, Kendall's correlation) between the variable of interest and the other variables. It can also compute the p-values permutation tests.}

\value{
  A data frame
}

\author{
  Nicolas Robette
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
  
  \seealso{
    \code{\link{assoc.yx}}, \code{\link{assoc.twocat}}, \code{\link{assoc.twocont}}, \code{\link{assoc.catcont}}, \code{\link{condesc}}, \code{\link{catdesc}}
  }

\examples{
  data(iris)
  iris2 = iris
  iris2$Species = factor(iris$Species == "versicolor")
  darma(iris2$Species, iris2[,1:4], target=2, nperm=100)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
