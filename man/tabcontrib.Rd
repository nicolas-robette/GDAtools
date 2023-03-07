\name{tabcontrib}

\alias{tabcontrib}

\title{Displays a fancy table with the main contributions of categories to an axis of a MCA}

\description{
Identifies the categories that contribute the most to a given dimension of a Multiple Correspondence 
Analysis and organizes these informations into a fancy table.
}

\usage{tabcontrib(resmca, dim = 1, best = TRUE, dec = 2)}

\arguments{
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, or \code{csMCA}}
  \item{dim}{dimension to describe (default is 1st dimension)}
  \item{best}{if FALSE, displays all the categories; if TRUE (default), displays only categories with contributions higher than average}
  \item{dec}{integer. The number of decimals for the results (default is 2)}
}

\details{
It allows to analyze variants of MCA, such as 'specific' MCA or 'class specific' MCA.
}

\value{
A data frame with the following contributions.: 
  \item{Variable}{names of the variables}
  \item{Category}{names of the categories}
  \item{Weight}{weights of the categories}
  \item{Contribution (left)}{contributions of the categories located on one side of the axis}
  \item{Contribution (right)}{contributions of the categories located on the other side of the axis}
  \item{Total contribution}{contributions summed by variable}
  \item{Cumulated contribution}{cumulated sum of the contributions}
  \item{Contribution of deviation}{for each variable, contribution of the deviation between the barycenter of the categories located on one side of
the axis and the barycenter of those located on the other side}
  \item{Proportion to variable}{contribution of deviation expressed as a proportion of the contribution of the variable}
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
