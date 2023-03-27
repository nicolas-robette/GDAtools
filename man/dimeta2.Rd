\name{dimeta2}

\alias{dimeta2}

\title{Correlation ratios (aka eta-squared) of supplementary variables}

\description{
Computes correlation ratios (also known as eta-squared) for a list of supplementary variables of a MCA.
}

\usage{dimeta2(resmca, vars, dim = c(1,2))}

\arguments{
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}}
  \item{vars}{a data frame of supplementary variables}
  \item{dim}{the axes for which eta2 are computed. Default is c(1,2)}
}

\value{
  Returns a data frame with supplementary variables as rows and MCA axes as columns.
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{dimdescr}}, \code{\link{dimcontrib}}, \code{\link{dimtypicality}}
}

\examples{
# specific MCA on Music example data set
data(Music)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA")
mca <- speMCA(Music[,1:5], excl = junk)
# correlation ratios
dimeta2(mca, Music[, c("Gender", "Age")])
}

\keyword{ multivariate }
\keyword{ misc }
