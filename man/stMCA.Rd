\name{stMCA}

\alias{stMCA}

\title{Standardized MCA}

\description{Performs a standardized Multiple Correspondence Analysis, i.e it takes MCA results and forces all the dimensions to be orthogonal to a supplementary "control" variable.}

\usage{stMCA(resmca, control)}

\arguments{
  \item{resmca}{an object of class \code{MCA}, \code{speMCA}, \code{csMCA} or \code{multiMCA}}
  \item{control}{a list of control variables}
}

\details{
Standardized MCA unfolds in several steps.
1. First, for each dimension of an input MCA, individual coordinates are used as dependent variable in a linear regression model and the 'control' variable is included as covariate in the same model.
2. The residuals from every models are retained and bound together. The resulting data frame is composed of continuous variables and its number of columns is equal to the number of dimensions in the input MCA.
3. Lastly, this data frame is used as input in a Principal Component Analysis.

It is exactly equivalent to MCA with one orthogonal instrumental variable (see \code{\link{MCAoiv}})
}

\value{
Returns an object of class \code{stMCA}. This object will be similar to \code{resmca} argument, still it does not comprehend modified rates, categories contributions and variables contributions.
}

\references{
Bry X., Robette N., Roueff O., 2016, « A dialogue of the deaf in the statistical theater? Adressing structural effects within a geometric data analysis framework », \emph{Quality & Quantity}, 50(3), pp 1009–1020 [https://link.springer.com/article/10.1007/s11135-015-0187-z]
}

\author{Nicolas Robette}

\seealso{
\code{\link{plot.stMCA}}
}

\examples{
# standardized MCA of Music example data set
# controlling for age
## and then draws the cloud of categories.
data(Music)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA")
mca <- speMCA(Music[,1:5], excl = junk)
stmca <- stMCA(mca, control = list(Music$Age))
}

\keyword{ multivariate }
