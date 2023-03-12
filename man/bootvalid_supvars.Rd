\name{bootvalid_supvars}
\alias{bootvalid_supvars}

\title{Bootstrap validation for MCA (supplementary variables)}

\description{
Bootstrap validation of MCA, through the computation of the coordinates of supplementary variables for bootstrap replications of the data.
}

\usage{
bootvalid_supvars(resmca, vars = NULL, axes = c(1,2), K = 30)
}

\arguments{
  \item{resmca}{object of class \code{speMCA}.}
  \item{vars}{A data frame of categorical supplementary variables. All these variables should be factors.}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{K}{integer. Number of bootstrap replications (default is 30).}
}

\details{
The bootstrap technique is used here as an internal (and non-parametric) validation procedure of the results of a multiple correspondence analysis. For supplementary variables, only partial bootstrap is possible. The partial bootstrap does not compute new MCAs: it projects bootstrap replications of the initial data as supplementary elements of the MCA. 
See references for more details.
}

\value{
A data frame with the following elements :
\item{varcat}{Name of the active category}
\item{K}{Index of the bootstrap replication}
\item{dim.x}{Bootstrap coordinate on the first selected axis}
\item{dim.y}{Bootstrap coordinate on the second selected axis}
}

\references{
Lebart L. (2006). "Validation Techniques in Multiple Correspondence Analysis". In M. Greenacre et J. Blasius (eds), \emph{Multiple Correspondence Analysis and related techniques}, Chapman and Hall/CRC, p.179-196.

Lebart L. (2007). "Which bootstrap for principal axes methods?". In P. Brito et al. (eds), \emph{Selected Contributions in Data Analysis and Classification}, Springer, p.581-588.
}

\author{Nicolas Robette}

\seealso{
\code{\link{bootvalid_variables}}
}

\examples{
data(Taste)
resmca <- speMCA(Taste[,1:11], excl=c(3,6,9,12,15,18,21,24,27,30,33))
supvars <- Taste[,c("Gender", "Age", "Educ")]
bootvalid_supvars(resmca, supvars, K = 5)
}

\keyword{ multivariate }
\keyword{ misc }
