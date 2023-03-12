\name{ggbootvalid_supvars}
\alias{ggbootvalid_supvars}

\title{Ellipses for bootstrap validation for MCA (supplementary variables)}

\description{
Ellispes for bootstrap validation of MCA, through the computation of the coordinates of supplementary variables for bootstrap replications of the data.
}

\usage{
ggbootvalid_supvars(resmca, vars = NULL, axes = c(1,2), K = 30,
                    ellipse = "norm", level = 0.95,
                    active = FALSE, palette = NULL, legend = "right")
}

\arguments{
  \item{resmca}{object of class \code{speMCA}.}
  \item{vars}{A data frame of categorical supplementary variables. All these variables should be factors.}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{K}{integer. Number of bootstrap replications (default is 30).}
  \item{ellipse}{character string. The type of ellipse. The default "norm" assumes a multivariate normal distribution, "t" assumes a multivariate t-distribution, and "euclid" draws a circle with the radius equal to level, representing the euclidean distance from the center.}
  \item{level}{numerical value. The level at which to draw an ellipse, or, if \code{ellipse}="euclid", the radius of the circle to be drawn.}
  \item{active}{logical. If TRUE, the labels of active variables are added to the plot in lightgray. Default is FALSE.}
  \item{palette}{Character string or character vector. Colors for the ellipses and labels of the categories, differentiated by variable. Can be the name of a palette from the \code{RcolorBrewer} package, a character vector of colors for a custom palette or a single color name (for no difference between variables). If NULL (default), the default palette of \code{ggplot2} is used.}
  \item{legend}{the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector). Default is right.}  
}

\details{
The bootstrap technique is used here as an internal (and non-parametric) validation procedure of the results of a multiple correspondence analysis. For supplementary variables, only partial bootstrap is possible. The partial bootstrap does not compute new MCAs: it projects bootstrap replications of the initial data as supplementary elements of the MCA. 
See references for more details.

The default parameters for ellipses assume a multivariate normal distribution drawn at level 0.95.
}

\value{a \code{ggplot2} object}

\references{
Lebart L. (2006). "Validation Techniques in Multiple Correspondence Analysis". In M. Greenacre et J. Blasius (eds), \emph{Multiple Correspondence Analysis and related techniques}, Chapman and Hall/CRC, p.179-196.

Lebart L. (2007). "Which bootstrap for principal axes methods?". In P. Brito et al. (eds), \emph{Selected Contributions in Data Analysis and Classification}, Springer, p.581-588.
}

\author{Nicolas Robette}

\seealso{
\code{\link{bootvalid_supvars}}
}

\examples{
data(Taste)
resmca <- speMCA(Taste[,1:11], excl=c(3,6,9,12,15,18,21,24,27,30,33))
supvars <- Taste[,c("Gender", "Age", "Educ")]
ggbootvalid_supvars(resmca, supvars)
}

\keyword{ multivariate }
\keyword{ aplot }
