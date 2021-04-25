\name{dimcontrib}

\alias{dimcontrib}

\title{Describes the contributions to axes for a MCA}

\description{
Identifies the categories and individuals that contribute the most to each dimension obtained 
by a Multiple Correspondence Analysis. It allows to analyze variants of MCA, such as 'specific' MCA or 'class specific' MCA.
}

\usage{dimcontrib(resmca, dim = c(1,2), best = TRUE)}

\arguments{
  \item{resmca}{object of class \code{'MCA'}, \code{'speMCA'}, or \code{'csMCA'}}
  \item{dim}{dimensions to describe (default is c(1,2))}
  \item{best}{if FALSE, displays all the categories; if TRUE (default), displays only categories and individuals with contributions higher than average}
}

\details{
Contributions are sorted and assigned a positive or negative sign according to the corresponding categories or individuals' 
coordinates, so as to facilitate interpretation.
}

\value{
  Returns a list: 
  \item{var}{a list of categories' contributions to axes}
  \item{ind}{a list of individuals' contributions to axes}
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{dimdescr}}, \code{\link{dimeta2}}, \code{\link{dimtypicality}}, \code{\link{condes}}, \code{\link{speMCA}}, \code{\link{csMCA}}
}

\examples{
## Performs a specific MCA on 'Music' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## and then describes the contributions to axes.
data(Music)
getindexcat(Music[,1:5])
mca <- speMCA(Music[,1:5],excl=c(3,6,9,12,15))
dimcontrib(mca)
}

\keyword{ multivariate }
\keyword{ misc }
