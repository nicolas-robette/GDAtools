\name{assoc.twocont}
\alias{assoc.twocont}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Measures the association between two continuous variables}
\description{
Measures the association between two continuous variables with Pearson, Spearman and Kendall correlations.
}
\usage{
assoc.twocont(x,y,weights=rep(1,length(x)),
              nperm=NULL,distrib="asympt")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a continuous variable (must be a numeric vector)}
  \item{y}{a continuous variable (must be a numeric vector)}
  \item{weights}{an optional numeric vector of weights (by default, a vector of 1 for uniform weights)}
  \item{nperm}{numeric. Number of permutations for the permutation test of independence. If NULL (default), no permutation test is performed.}
  \item{distrib}{the null distribution of permutation test of independence can be approximated by its asymptotic distribution (\code{"asympt"}, default) or via Monte Carlo resampling (\code{"approx"}.}
}

\value{
A data frame with Pearson, Spearman and Kendall correlations. The correlation value is in the first row and a p-value from a permutation (so non parametric) test of independence is in the second row.
}

\author{Nicolas Robette}

\seealso{
\code{\link{assoc.twocat}}, \code{\link{assoc.catcont}}, \code{\link{assoc.yx}}, \code{\link{condesc}},
\code{\link{catdesc}}, \code{\link{darma}}, \code{\link{ggassoc_scatter}}
}
\examples{
## Hollander & Wolfe (1973), p. 187f.
## Assessment of tuna quality.  We compare the Hunter L measure of
##  lightness to the averages of consumer panel scores (recoded as
##  integer values from 1 to 6 and averaged over 80 such values) in
##  9 lots of canned tuna.
x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
assoc.twocont(x,y,nperm=100)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ misc }
