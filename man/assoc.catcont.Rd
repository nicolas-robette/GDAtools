\name{assoc.catcont}
\alias{assoc.catcont}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Measures the association between a categorical variable and a continuous variable}
\description{
Measures the association between a categorical variable and a continuous variable
}
\usage{
assoc.catcont(x,y,weights=rep(1,length(x)),
              nperm=1000,distrib="asympt",digits=3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the categorical variable (must be a factor)}
  \item{y}{the continuous variable (must be a numeric vector)}
  \item{weights}{an optional numeric vector of weights (by default, a vector of 1 for uniform weights)}
  \item{nperm}{numeric. Number of permutations for the permutation test of independence. If NULL, no permutation test is performed.}
  \item{distrib}{the null distribution of permutation test of independence can be approximated by its asymptotic distribution (\code{"asympt"}, default) or via Monte Carlo resampling (\code{"approx"}.}
  \item{digits}{integer. The number of digits (default is 3).}
}

\value{
A list with the following elements :
\item{eta.squared}{eta-squared between the two variables}
\item{permutation.pvalue}{p-value from a permutation (so non-parametric) test of independence}
\item{cor.coeff}{correlation coefficient between the two variables, for each level of the categorical variable}
\item{cor.perm.pval}{permutation p-value of the correlation coefficient between the two variables, for each level of the categorical variable}
}
\references{
Rakotomalala R., 'Comprendre la taille d'effet (effect size)', [http://eric.univ-lyon2.fr/~ricco/cours/slides/effect_size.pdf]
}
\author{Nicolas Robette}

\seealso{
\code{\link{assoc.twocat}}, \code{\link{assoc.twocont}}, \code{\link{assoc.yx}}, \code{\link{condesc}}, \code{\link{catdesc}}
}
\examples{
## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
assoc.catcont(group,weight,nperm=100)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ misc }
