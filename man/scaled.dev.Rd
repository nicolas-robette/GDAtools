\name{scaled.dev}

\alias{scaled.dev}

\title{Scaled deviations for a categorical supplementary variable}

\description{
   From MCA results, computes scaled deviations between categories for 
   a categorical supplementary variable.
}

\usage{
scaled.dev(resmca, var)
}

\arguments{
  \item{resmca}{object created with \code{MCA}, \code{speMCA}, \code{csMCA}, \code{wcMCA}, \code{bcMCA}, \code{stMCA} or \code{multiMCA} function}
  \item{var}{the categorical supplementary variable. It does not need to have been used at the MCA step.}
}

\value{
  Returns a list with one matrix for each dimension of the MCA.
  Each matrix is filled with scaled deviations between the categories of the supplementary variable, for a given dimension.
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{supvar}}, \code{\link{supvars}}, \code{\link{ggadd_supvar}}, \code{\link{ggadd_supvars}}, \code{\link{textvarsup}}, \code{\link{supind}}
}

\examples{
# specific MCA of Music example data set
data(Music)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA")
mca <- speMCA(Music[,1:5], excl = junk)
# computes scaled deviations for Age supplementary variable
scaled.dev(mca,Music$Age)
}

\keyword{ multivariate }
\keyword{ misc }
