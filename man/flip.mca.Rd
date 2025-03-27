\name{flip.mca}

\alias{flip.mca}

\title{Flips the coordinates}

\description{Flips the coordinates of the individuals and the categories on one or more dimensions of a MCA.}

\usage{flip.mca(resmca, dim = 1)}

\arguments{
  \item{resmca}{object created with \code{MCA}, \code{speMCA}, \code{csMCA}, \code{wcMCA}, \code{bcMCA}, \code{stMCA} or \code{multiMCA} function}
  \item{dim}{numerical vector of the dimensions for which the coordinates are flipped. By default, only the first dimension is flipped}
}

\value{
  Returns an object of the same class as \code{resmca}
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{ggcloud_variables}}, \code{\link{ggcloud_indiv}}
}

\examples{
# MCA of Music example data set
data(Music)
mca <- speMCA(Music[,1:5])
ggcloud_variables(mca, legend = "none")
# Flips dimensions 1 and 2
flipped_mca <- flip.mca(mca, dim = c(1,2))
ggcloud_variables(flipped_mca, legend = "none")
}

\keyword{ multivariate }
\keyword{ misc }
