\name{phi.table}
\alias{phi.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Computes the phi coefficient for every cells of a contingency table}
\description{
Computes the phi coefficient for every cells of the cross-tabulation between two categorical variables
}
\usage{
phi.table(x,y,weights=rep(1,length(x)),digits=3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the first categorical variable}
  \item{y}{the second categorical variable}
  \item{weights}{an optional numeric vector of weights (by default, a vector of 1 for uniform weights)}
  \item{digits}{integer. The number of digits (default is 3).}
}

\value{
A table with the phi coefficients
}
\references{
Rakotomalala R., 'Comprendre la taille d'effet (effect size)', http://eric.univ-lyon2.fr/~ricco/cours/slides/effect_size.pdf
}
\author{Nicolas Robette}

\seealso{
\code{\link{assoc.twocat}},\code{\link{assoc.catcont}}, \code{\link{condesc}}, \code{\link{catdesc}}
}
\examples{
data(Music)
phi.table(Music$Jazz,Music$Age)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ misc }
