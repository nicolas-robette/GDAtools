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
\details{
%%  ~~ If necessary, more details than the description above ~~
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
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{catdes}}, \code{\link[moreparty]{BivariateAssoc}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ misc }
