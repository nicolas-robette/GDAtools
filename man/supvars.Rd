\name{supvars}

\alias{supvars}
\alias{varsups}

\title{Statistics for categorical supplementary variables}

\description{
   From MCA results, computes statistics (weights, coordinates, squared cosines, contributions, test-values, variances) for categorical supplementary variables.
}

\usage{
supvars(resmca, vars)

varsups(resmca, vars)
}

\arguments{
  \item{resmca}{object created with \code{MCA}, \code{speMCA}, \code{csMCA}, \code{wcMCA}, \code{bcMCA}, \code{stMCA} or \code{multiMCA} function}
  \item{vars}{A data frame of categorical supplementary variables. All these variables should be factors.}
}

\value{
  Returns a list with the following items :
  \item{weight}{numeric vector of categories weights}
  \item{coord}{data frame of categories coordinates}
  \item{cos2}{data frame of categories squared cosines}
  \item{var}{a list of data frames of categories within variances, variance between and within categories and variable square correlation ratio (eta2)}
  \item{typic}{data frame of categories typicality test statistics}
  \item{pval}{data frame of categories p-values from typicality test statistics}
  \item{cor}{data frame of categories correlation coefficients}  
}

\note{
\code{varsups} is softly deprecated. Please use \code{supvars} instead.
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{supvar}}, \code{\link{ggadd_supvar}}, \code{\link{ggadd_supvars}}, \code{\link{textvarsup}}, \code{\link{supind}}
}

\examples{
# specific MCA of Music example data set
data(Music)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA")
mca <- speMCA(Music[,1:5], excl = junk)
# computes statistics for Gender and Age supplementary variables
supvars(mca, Music[, c("Gender","Age")])
}

\keyword{ multivariate }
\keyword{ misc }
