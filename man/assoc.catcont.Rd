\name{assoc.catcont}
\alias{assoc.catcont}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Bivariate association}
\description{
Measures the association between a categorical variable and a continuous variable
}
\usage{
assoc.catcont(x,y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the categorical variable (must be a factor)}
  \item{y}{the continuous variable (must be a factor)}
}

\value{
A list with the following elements :
\item{eta.squared}{eta-squared between the two variables}
\item{cor.coeff}{correlation coefficient between the two variables, for each level of the categorical variable}
}
\references{
Rakotomalala R., 'Comprendre la taille d'effet (effect size)', [http://eric.univ-lyon2.fr/~ricco/cours/slides/effect_size.pdf]
}
\author{Nicolas Robette}

\seealso{
\code{\link{assoc.twocat}}, \code{\link{condesc}}, \code{\link{catdesc}}
}
\examples{
data(Taste)
getindexcat(Taste[,1:5])
mca <- speMCA(Taste[,1:5],excl=c(3,6,9,12,15))
assoc.catcont(Taste$Gender,mca$ind$coord[,1])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ misc }
