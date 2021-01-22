\name{assoc.twocat}
\alias{assoc.twocat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cross-tabulation and measures of association between two categorical variables}
\description{
Cross-tabulation and measures of association between two categorical variables
}
\usage{
assoc.twocat(x,y,weights=rep.int(1,length(x)),na=TRUE,nperm=1000,distrib="asympt")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the first categorical variable (must be a factor)}
  \item{y}{the second categorical variable (must be a factor)}
  \item{weights}{an optional numeric vector of weights (by default, a vector of 1 for uniform weights)}
  \item{na}{logical. If TRUE (default), 'NA' are treated as a category. If FALSE, they are ignored}
  \item{nperm}{numeric. Number of permutations for the permutation test of independence. If NULL, no permutation test is performed. Default is 1000.}
  \item{distrib}{the null distribution of permutation test of independence can be approximated by its asymptotic distribution (\code{"asympt"}, default) or via Monte Carlo resampling (\code{"approx"}.}
 
}

\value{
A list with the following elements :
\item{freq}{cross-tabulation}
\item{prop}{percentages}
\item{rprop}{raw percentages}
\item{cprop}{column percentages}
\item{expected}{expected values}
\item{chi.squared}{chi-squared value}
\item{cramer.v}{Cramer's V between the two variables}
\item{permutation.pvalue}{p-value from a permutation (so non-parametric) test of independence}
\item{pearson.residuals}{the table of Pearson residuals, i.e. (observed - expected) / sqrt(expected).}
\item{phi}{the table of the phi coefficients for each pair of levels}
\item{phi.perm.pval}{the table of permutation p-values for each pair of levels}
}
\references{
Rakotomalala R., 'Comprendre la taille d'effet (effect size)', http://eric.univ-lyon2.fr/~ricco/cours/slides/effect_size.pdf
}
\author{Nicolas Robette}

\seealso{
\code{\link{assoc.catcont}}, \code{\link{assoc.twocont}}, \code{\link{assoc.yx}}, \code{\link{condesc}}, \code{\link{catdesc}}
}
\examples{
data(Music)
assoc.twocat(Music$Jazz,Music$Age,nperm=100)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ misc }
