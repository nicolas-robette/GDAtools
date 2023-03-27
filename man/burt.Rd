\name{burt}

\alias{burt}

\title{Burt table}

\description{Computes a Burt table from a data frame composed of categorical variables.}

\usage{burt(data)}

\arguments{
  \item{data}{data frame with n rows (individuals) and p columns (categorical variables)}
}

\details{
A Burt table is a symmetric table that is used in correspondence analysis. 
It shows the frequencies for all combinations of categories of pairs of variables.
}

\value{Returns a square matrix. Its dimension is equal to the total number of categories in the data frame.}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
 \code{\link{dichotom}}
}

\examples{
## Burt table of variables in columns 1 to 5
## in the Music example data set
data(Music)
burt(Music[,1:5])
}

\keyword{ multivariate }
\keyword{ misc }
