\name{assoc.twocat}
\alias{assoc.twocat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Bivariate association}
\description{
Cross-tabulation and measures of association between two categorical variables
}
\usage{
assoc.twocat(x,y,w=rep.int(1,length(x)),na=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the first categorical variable (must be a factor)}
  \item{y}{the second categorical variable (must be a factor)}
  \item{w}{an optional numeric vector of weights (by default, a vector of 1 for uniform weights)}
  \item{na}{logical. If TRUE (default), 'NA' are treated as a category. If FALSE, they are ignored}
}

\value{
A list with the following elements :
\item{freq}{cross-tabulation}
\item{prop}{percentages}
\item{rprop}{raw percentages}
\item{cprop}{column percentages}
\item{v2.cramer}{Cramer's V2 between the two variables}
\item{phi}{the matrix of the phi values for each pair of levels}
}
\references{
Rakotomalala R., 'Comprendre la taille d'effet (effect size)', http://eric.univ-lyon2.fr/~ricco/cours/slides/effect_size.pdf
}
\author{Nicolas Robette}

\seealso{
\code{\link{assoc.catcont}}, \code{\link{condesc}}, \code{\link{catdesc}}
}
\examples{
data(Music)
assoc.twocat(Music$Jazz,Music$Age)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ misc }
