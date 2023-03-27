\name{angles.csa}

\alias{angles.csa}

\title{Cosine similarities and angles between CSA and MCA}

\description{Computes the cosines similarities and angles between the components of a CSA and those of a MCA.}

\usage{
angles.csa(rescsa, resmca)
}

\arguments{
  \item{rescsa}{object of class \code{csMCA}}
  \item{resmca}{object of class \code{MCA} or \code{speMCA}}
}

\note{
This function is adapted from \code{csa.measures} in \code{sco.ca} package.
}

\value{
  A list of matrices:
  \item{cosines}{Cosine similarities}
  \item{angles}{Angles}
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{MCA}}, \code{\link{speMCA}}, \code{\link{csMCA}}
}

\examples{
## Performs a specific MCA and a CSA on the Music example data set
## and computes cosine similarities and angles
data(Music)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA")
resmca <- speMCA(Music[,1:5], excl = junk)
female <- Music$Gender=="Women"
rescsa <- csMCA(Music[,1:5], subcloud = female, excl = junk)
angles.csa(rescsa, resmca)
}

\keyword{ multivariate }
\keyword{ misc }
