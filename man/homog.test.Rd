\name{homog.test}

\alias{homog.test}

\title{Homogeneity test for a categorical supplementary variable}

\description{
   From MCA results, computes a homogeneity test between categories of a supplementary variable, i.e. 
   characterizes the homogeneity of several subclouds.
}

\usage{homog.test(resmca, var, dim = c(1,2))}

\arguments{
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}}
  \item{var}{the categorical supplementary variable. It does not need to have been used at the MCA step.}
  \item{dim}{the axes which are described. Default is c(1,2)}
}

\value{
  Returns a list of lists, one for each selected dimension in the MCA.
  Each list has 2 elements :
  \item{test.stat}{The square matrix of test statistics}
  \item{p.values}{The square matrix of p-values}
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{supvar}}, \code{\link{supvars}}, \code{\link{dimtypicality}}
}

\examples{
# specific MCA of Music example data set
data(Music)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA")
mca <- speMCA(Music[,1:5], excl = junk)
# homogeneity test for variable Age
homog.test(mca, Music$Age)
}

\keyword{ multivariate }
\keyword{ misc }
