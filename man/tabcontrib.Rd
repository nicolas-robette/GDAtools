\name{tabcontrib}

\alias{tabcontrib}

\title{Displays the categories contributing most to axes for a MCA}

\description{
Identifies the categories that contribute the most to a given dimension of a Multiple Correspondence 
Analysis and organizes these informations into a fancy table. It allows to analyze variants of MCA, such as 'specific' MCA or 'class specific' MCA.
}

\usage{tabcontrib(resmca, dim = 1)}

\arguments{
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, or \code{csMCA}}
  \item{dim}{dimension to describe (default is 1st dimension)}
}

\details{
Best contributions - i.e. higher than average - are assigned a positive or negative sign according to the corresponding categories' 
coordinates, so as to facilitate interpretation. Then they are sorted and organized according to the most contributing variables.
}

\value{
  Returns a data frame with the following columns: 
  \item{var}{the names of the most contributing variables}
  \item{moda}{the names of the most contributing cateories}
  \item{ctr1}{'negative' contributions, i.e. corresponding to categories with coordinates lower than zero}
  \item{ctr2}{'positive' contributions, i.e. corresponding to categories with coordinates higher than zero}
  \item{weight}{weight of the categories}
  \item{ctrtot}{sum of the best contributions for a given variable}
  \item{cumctrtot}{cumulated contributions}
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{dimcontrib}}, \code{\link{dimdesc}}, \code{\link{dimdescr}}, \code{\link{dimeta2}}, \code{\link{condes}}, \code{\link{speMCA}}, \code{\link{csMCA}}
}

\examples{
## Performs a specific MCA on 'Music' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## and then describes the contributions to axes.
data(Music)
getindexcat(Music[,1:5])
mca <- speMCA(Music[,1:5],excl=c(3,6,9,12,15))
tabcontrib(mca,1)
tabcontrib(mca,2)
}

\keyword{ multivariate }
\keyword{ misc }
