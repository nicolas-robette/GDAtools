\name{dimtypicality}

\alias{dimtypicality}

\title{Typicality tests for supplementary variables of a MCA}

\description{
Computes typicality tests for a list of supplementary variables. It allows to analyze variants of MCA, such as 'specific' MCA or 'class specific' MCA.
}

\usage{dimtypicality(resmca, vars, dim = c(1,2), max.pval=1)}

\arguments{
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}}
  \item{vars}{a data frame of supplementary variables}
  \item{dim}{the axes for which typicality tests are computed. Default is c(1,2)}
  \item{max.pval}{only categories with a p-value lower or equal to max.pval are displayed. By default, all categories are displayed}
}

\value{
  Returns a list of data frames giving the test statistics and p-values of the supplementary categories for the different axes.
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{dimdesc}}, \code{\link{dimdescr}}, \code{\link{dimeta2}}, \code{\link{dimcontrib}}, \code{\link{condes}}, \code{\link{speMCA}}, \code{\link{csMCA}}
}

\examples{
## Performs a specific MCA on 'Music' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## and then computes the typicality tests for Gender and Age (axes 1 and 2).
data(Music)
getindexcat(Music[,1:5])
mca <- speMCA(Music[,1:5],excl=c(3,6,9,12,15))
dimtypicality(mca, Music[,c("Gender","Age")])
}

\keyword{ multivariate }
\keyword{ misc }
