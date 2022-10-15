\name{oddsratio.table}
\alias{oddsratio.table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Computes the odds ratios for every cells of a contingency table}
\description{
Computes the odds ratio for every cells of the cross-tabulation between two categorical variables
}
\usage{
oddsratio.table(x,y,weights=rep(1,length(x)),digits=3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{the first categorical variable}
  \item{y}{the second categorical variable}
  \item{weights}{an optional numeric vector of weights (by default, a vector of 1 for uniform weights)}
  \item{digits}{integer. The number of digits (default is 3). If NULL, the results are not rounded.}
}

\value{
A table with the odds ratios
}

\author{Nicolas Robette}

\seealso{
\code{\link{assoc.twocat}},\code{\link{assoc.catcont}}, \code{\link{condesc}}, \code{\link{catdesc}}
}
\examples{
data(Music)
oddsratio.table(Music$Jazz,Music$Age)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multivariate }
\keyword{ misc }
