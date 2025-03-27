\name{dimcontrib}

\alias{dimcontrib}

\title{Description of the contributions to axes}

\description{
Identifies the categories and individuals that contribute the most to each dimension obtained by a Multiple Correspondence Analysis.
}

\usage{dimcontrib(resmca, dim = c(1,2), best = TRUE)}

\arguments{
  \item{resmca}{object created with \code{MCA}, \code{speMCA}, \code{csMCA}, \code{wcMCA} or \code{bcMCA} function}
  \item{dim}{numerical vector of the dimensions to describe (default is c(1,2))}
  \item{best}{logical. If FALSE, displays all the categories. If TRUE (default), displays only categories and individuals with contributions higher than average}
}

\details{
Contributions are sorted and assigned a positive or negative sign according to the corresponding categories or individuals coordinates, so as to facilitate interpretation.
}

\note{
Contributions of individuals cannot be computed for objects created by \code{wcMCA} function.
}

\value{
Returns a list with the following items : 
  \item{var}{a list of categories contributions to axes}
  \item{ind}{a list of individuals contributions to axes}
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{tabcontrib}}, \code{\link{dimdescr}}, \code{\link{dimeta2}}, \code{\link{dimtypicality}}
}

\examples{
# specific MCA on Music example data set
data(Music)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA")
mca <- speMCA(Music[,1:5], excl = junk)
# contributions to axes 1 and 2
dimcontrib(mca)
}

\keyword{ multivariate }
\keyword{ misc }
