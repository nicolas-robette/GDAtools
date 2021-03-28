\name{condesc}
\alias{condesc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Measures the association between a continuous variable and some continuous and/or categorical variables}
\description{
Measures the association between a continuous variable and some continuous and/or categorical variables}
\usage{
condesc(y,x,min.cor=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{the continuous variable to describe}
  \item{x}{a data frame with continuous and/or categorical variables}
  \item{min.cor}{for the relationship between y and a categorical variable, only associations higher or equal to min.cor will be displayed. If NULL (default), they are all displayed.}
}

\value{
A list of the following items :
\item{variables}{associations between y and the variables in x, computed with BivariateAssoc function from package moreparty}
\item{categories}{a data frame with categorical variables from x and associations measured by correlation coefficients}
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
data(Taste)
getindexcat(Taste[,1:5])
mca <- speMCA(Taste[,1:5],excl=c(3,6,9,12,15))
condesc(mca$ind$coord[,1], Taste[,c('Gender','Age')])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ misc }
