\name{varsups}

\alias{varsups}

\title{Computes statistics for categorical supplementary variables}

\description{
   From MCA results, computes statistics (weights, coordinates, contributions, test-values, variances) for 
   categorical supplementary variables.
}

\usage{varsups(resmca, vars)}

\arguments{
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}}
  \item{vars}{A data frame of categorical supplementary variables. All these variables should be factors.}
}

\value{
  Returns a list:
  \item{weight}{numeric vector of categories weights}
  \item{coord}{data frame of categories coordinates}
  \item{cos2}{data frame of categories square cosine}
  \item{var}{a list of data frames of categories within variances, variance between and within categories and variable square correlation ratio (eta2)}
  \item{typic}{data frame of categories typicality test statistics}
  \item{pval}{data frame of categories p-values from typicality test statistics}
  \item{cor}{data frame of categories correlation coefficients}  
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{varsup}}, \code{\link{speMCA}}, \code{\link{csMCA}}, \code{\link{multiMCA}}, \code{\link{textvarsup}}
}

\examples{
## Performs a specific MCA on 'Music' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## and then computes statistics for supplementary variables.
data(Music)
getindexcat(Music)
mca <- speMCA(Music[,1:5],excl=c(3,6,9,12,15))
varsups(mca,Music[,6:9])
}

\keyword{ multivariate }
\keyword{ misc }
