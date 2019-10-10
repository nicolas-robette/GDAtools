\name{csMCA}

\alias{csMCA}

\title{Performs a 'class specific' MCA}

\description{Performs a 'class specific' Multiple Correspondence Analysis, i.e. a variant of MCA consisting in analyzing a subcloud of individuals.}

\usage{
csMCA(data, subcloud = rep(TRUE, times = nrow(data)), excl = NULL, ncp = 5, 
row.w = rep(1, times = nrow(data)))
}

\arguments{
  \item{data}{data frame with n rows (individuals) and p columns (categorical variables)}
  \item{subcloud}{a vector of logical values and length n. The subcloud of individuals analyzed with 'class specific' MCA
                  is made of the individuals with value \code{TRUE}.}
  \item{excl}{numeric vector indicating the indexes of the 'junk' categories (default is NULL). See \code{\link{getindexcat}} to identify these indexes.}
  \item{ncp}{number of dimensions kept in the results (default is 5)}
  \item{row.w}{an optional numeric vector of row weights (by default, a vector of 1 for uniform row weights)}
}

\details{
This variant of MCA is used to study a subset of individuals with reference to the whole set of individuals, i.e. to determine 
the specific features of the subset. It consists in proceeding to the search of the principal axes of the subcloud associated 
with the subset of individuals (see Le Roux and Rouanet, 2004 and 2010).
}

\value{
Returns an object of class \code{'csMCA'}, i.e. a list including:
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
\code{\link{getindexcat}}, \code{\link{plot.csMCA}}, \code{\link{varsup}}, \code{\link{contrib}}, 
\code{\link{modif.rate}}, \code{\link{dimdesc.MCA}}, \code{\link{speMCA}}, \code{\link{MCA}}
}

\examples{
## Performs a 'class specific' MCA on 'Music' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## and focusing on the subset of women.
data(Music)
female <- Music$Gender=='Women'
mca <- csMCA(Music[,1:5],subcloud=female,excl=c(3,6,9,12,15))
plot(mca)
}

\keyword{ multivariate }
