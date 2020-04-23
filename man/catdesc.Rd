\name{catdesc}
\alias{catdesc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Bivariate associations}
\description{
Measures the association between a categorical variable and some continuous and/or categorical variables}
\usage{
catdesc(y,x,min.phi=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{the categorical variable to describe (must be a factor)}
  \item{x}{a data frame with continuous and/or categorical variables}
  \item{min.phi}{for the relationship between y and a categorical variable, only associations higher or equal to min.phi will be displayed. If NULL (default), they are all displayed.}
}

\value{
A list of the following items :
\item{variables}{associations between y and the variables in x, computed with BivariateAssoc function from package moreparty}
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
\code{\link{catdes}}, \code{\link[moreparty]{BivariateAssoc}}
}
\examples{
data(Taste)
getindexcat(Taste[,1:5])
mca <- speMCA(Taste[,1:5],excl=c(3,6,9,12,15))
hc <- hclust(dist(mca$ind$coord))
clust <- as.factor(cutree(hc,5))
catdesc(clust, Taste[,c('Gender','Age')])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ misc }
