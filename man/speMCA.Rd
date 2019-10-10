\name{speMCA}

\alias{speMCA}

\title{Performs a 'specific' MCA}

\description{Performs a 'specific' Multiple Correspondence Analysis, i.e. a variant of MCA that allows to treat undesirable categories as passive categories.}

\usage{speMCA(data, excl = NULL, ncp = 5, row.w = rep(1, times = nrow(data)))}

\arguments{
  \item{data}{data frame with n rows (individuals) and p columns (categorical variables)}
  \item{excl}{numeric vector indicating the indexes of the "junk" categories (default is NULL). See "getindexcat" to identify these indexes.}
  \item{ncp}{number of dimensions kept in the results (default is 5)}
  \item{row.w}{an optional numeric vector of row weights (by default, a vector of 1 for uniform row weights)}
}

\details{
Undesirable categories may be of several kinds: infrequent categories (say, <5 percents), heterogeneous categories (e.g. 'others') 
or uninterpretable categories (e.g. 'not available'). In these cases, 'specific' MCA may be useful to ignore these 
categories for the determination of distances between individuals (see Le Roux and Rouanet, 2004 and 2010).
}

\value{
Returns an object of class \code{'speMCA'}, i.e. a list including:
  \item{eig}{a list of vectors containing all the eigenvalues, the percentage of variance, the cumulative percentage of variance, the modified rates and the cumulative modified rates}
  \item{call}{a list with informations about input data}
  \item{ind}{a list of matrices containing the results for the individuals (coordinates, contributions)}
  \item{var}{a list of matrices containing all the results for the categories and variables (weights, coordinates, square cosine, categories contributions to axes and cloud, test values (v.test), square correlation ratio (eta2), variable contributions to axes and cloud}
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{getindexcat}}, \code{\link{plot.speMCA}}, \code{\link{varsup}}, \code{\link{contrib}}, 
\code{\link{modif.rate}}, \code{\link{dimdesc.MCA}}, \code{\link{MCA}}, \code{\link{csMCA}}
}

\examples{
## Performs a specific MCA on 'Music' example data set
## ignoring every 'NA' (i.e. 'not available') categories.
data(Music)
getindexcat(Music[,1:5])
mca <- speMCA(Music[,1:5],excl=c(3,6,9,12,15))
str(mca)
}

\keyword{ multivariate }
