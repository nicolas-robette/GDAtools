\name{supvar}

\alias{supvar}
\alias{varsup}

\title{Statistics for a categorical supplementary variable}

\description{
   From MCA results, computes statistics (weights, coordinates, contributions, test-values, variances) for 
   a categorical supplementary variable.
}

\usage{
supvar(resmca, var)

varsup(resmca, var)
}

\arguments{
  \item{resmca}{object created with \code{MCA}, \code{speMCA}, \code{csMCA}, \code{wcMCA}, \code{bcMCA}, \code{stMCA} or \code{multiMCA} function}
  \item{var}{the categorical supplementary variable. It does not need to have been used at the MCA step.}
}

\value{
  Returns a list:
  \item{weight}{numeric vector of categories weights}
  \item{coord}{data frame of categories coordinates}
  \item{cos2}{data frame of categories squared cosines}
  \item{var}{data frame of categories within variances, variance between and within categories and variable squared correlation ratio (eta2)}
  \item{typic}{data frame of categories typicality test statistics}
  \item{pval}{data frame of categories p-values from typicality test statistics}
  \item{cor}{data frame of categories correlation coefficients}  
}

\note{
\code{varsup} is softly deprecated. Please use \code{supvar} instead.
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{supvars}}, \code{\link{ggadd_supvar}}, \code{\link{ggadd_supvars}}, \code{\link{textvarsup}}, \code{\link{supind}}
}

\examples{
# specific MCA of Music example data set
data(Music)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA")
mca <- speMCA(Music[,1:5], excl = junk)
# computes statistics for Age supplementary variable
supvar(mca,Music$Age)
}

\keyword{ multivariate }
\keyword{ misc }
