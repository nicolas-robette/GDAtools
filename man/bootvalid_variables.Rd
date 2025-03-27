\name{bootvalid_variables}
\alias{bootvalid_variables}

\title{Bootstrap validation (active variables)}

\description{
Bootstrap validation of MCA, through the computation of the coordinates of active variables for bootstrap replications of the data.
}

\usage{
bootvalid_variables(resmca, axes = c(1,2), type = "partial", K = 30)
}

\arguments{
  \item{resmca}{object created with \code{MCA}, \code{speMCA}, \code{csMCA}, \code{wcMCA} or \code{bcMCA} function}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{type}{character string. Can be "partial", "total1", "total2" or "total3" (see details). Default is "partial".}
  \item{K}{integer. Number of bootstrap replications (default is 30).}
}

\details{
The bootstrap technique is used here as an internal and non-parametric validation procedure of the results of a multiple correspondence analysis. Following the work of Ludovic Lebart, several methods are proposed. The "total bootstrap" uses new MCAs computed from bootstrap replications of the initial data. In the type 1 total bootstrap (\code{type} = "total1"), the sign of the coordinates is corrected if necessary (the direction of the axes of an ACM being arbitrary). In type 2 (\code{type} = "total2"), the order of the axes and the sign of the coordinates are corrected if necessary. In type 3 (\code{type} = "total3"), a procrustean rotation is used to find the best superposition between the initial axes and the replicated axes.
The "partial bootstrap"" (\code{type} = "partial") does not compute new MCAs: it projects bootstrap replications of the initial data as supplementary elements of the MCA. It gives a more optimistic view of the stability of the results than the total bootstrap. It also runs faster. See references for more details, pros and cons of the various types, etc.
}

\value{
A data frame with the following elements :
\item{varcat}{Names of the active categories}
\item{K}{Indexes of the bootstrap replications}
\item{dim.x}{Bootstrap coordinates on the first selected axis}
\item{dim.y}{Bootstrap coordinates on the second selected axis}
}

\references{
Lebart L. (2006). "Validation Techniques in Multiple Correspondence Analysis". In M. Greenacre et J. Blasius (eds), \emph{Multiple Correspondence Analysis and related techniques}, Chapman and Hall/CRC, p.179-196.

Lebart L. (2007). "Which bootstrap for principal axes methods?". In P. Brito et al. (eds), \emph{Selected Contributions in Data Analysis and Classification}, Springer, p.581-588.
}

\author{Nicolas Robette}

\seealso{
\code{\link{ggbootvalid_variables}}, \code{\link{bootvalid_supvars}} 
}

\examples{
data(Taste)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA",
          "Comedy.NA", "Crime.NA", "Animation.NA", "SciFi.NA", "Love.NA", 
          "Musical.NA")
resmca <- speMCA(Taste[,1:11], excl = junk)
bv <- bootvalid_variables(resmca, type = "partial", K = 5)
str(bv)
}

\keyword{ multivariate }
\keyword{ misc }
