\name{modif.rate}

\alias{modif.rate}

\title{Benzecri's modified rates of variance}

\description{Computes Benzecri's modified rates of variance of a multiple correspondence analysis.}

\usage{modif.rate(resmca)}

\arguments{
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}}
}

\details{
As MCA clouds often have a high dimensionality, the variance rates of the first principle axes may be quite low, which makes them hard to interpret. Benzecri (1992, p.412) proposed to use \emph{modified rates} to better appreciate the relative importance of the principal axes.
}

\value{
  Returns a list of two data frames.
  The first one is called \code{raw} and has 3 variables:
  \item{eigen}{eigen values}
  \item{rate}{rates}
  \item{cum.rate}{cumulative rates}
  The second one is called \code{modif} and has 2 variables:
  \item{mrate}{modified rates}
  \item{cum.mrate}{cumulative modified rates}
}

\references{
Benzecri J.P., \emph{Correspondence analysis handbook}, New-York: Dekker (1992).

Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{MCA}}, \code{\link{speMCA}}, \code{\link{csMCA}}
}

\examples{
# MCA of Music' example data set
data(Music)
mca <- speMCA(Music[,1:5])
# modified rates of variance
modif.rate(mca)
}

\keyword{ multivariate }
\keyword{ misc }
