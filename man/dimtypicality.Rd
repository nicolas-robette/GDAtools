\name{dimtypicality}

\alias{dimtypicality}

\title{Typicality tests for supplementary variables}

\description{
Computes typicality tests for a list of supplementary variables of a MCA.
}

\usage{dimtypicality(resmca, vars, dim = c(1,2), max.pval = 1)}

\arguments{
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}}
  \item{vars}{a data frame of supplementary variables}
  \item{dim}{the axes for which typicality tests are computed. Default is c(1,2)}
  \item{max.pval}{only categories with a p-value lower or equal to \code{max.pval} are displayed. If 1 (default), all categories are displayed}
}

\value{
  Returns a list of data frames giving the typicality test statistics and p-values of the supplementary categories for the different axes.
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{dimdescr}}, \code{\link{dimeta2}}, \code{\link{dimcontrib}}
}

\examples{
# specific MCA on Music example data set
data(Music)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA")
mca <- speMCA(Music[,1:5], excl = junk)
# typicality tests for gender and age
dimtypicality(mca, Music[, c("Gender", "Age")])
}

\keyword{ multivariate }
\keyword{ misc }
