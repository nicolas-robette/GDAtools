\name{ggbootvalid_supvars}
\alias{ggbootvalid_supvars}

\title{Ellipses of bootstrap validation (supplementary variables)}

\description{
Ellipses for bootstrap validation of MCA, through the computation of the coordinates of supplementary variables for bootstrap replications of the data.
}

\usage{
ggbootvalid_supvars(resmca, vars = NULL, axes = c(1,2), K = 30,
                    ellipse = "norm", level = 0.95,
                    col = NULL, active = FALSE, legend = "right")
}

\arguments{
  \item{resmca}{object created with \code{MCA}, \code{speMCA}, \code{csMCA}, \code{wcMCA} or \code{bcMCA} function}
  \item{vars}{A data frame of categorical supplementary variables. All these variables should be factors.}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{K}{integer. Number of bootstrap replications (default is 30).}
  \item{ellipse}{character string. The type of ellipse. The default "norm" assumes a multivariate normal distribution, "t" assumes a multivariate t-distribution, and "euclid" draws a circle with the radius equal to level, representing the euclidean distance from the center.}
  \item{level}{numerical value. The level at which to draw an ellipse, or, if \code{ellipse}="euclid", the radius of the circle to be drawn.}
  \item{col}{Character string. Color name for the ellipses and labels of the categories. If NULL (default), the default \code{ggplot2} palette is used, with one color per variable.}
  \item{active}{logical. If TRUE, the labels of active variables are added to the plot in lightgray. Default is FALSE.}  
  \item{legend}{the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector). Default is right.}  
}

\details{
The bootstrap technique is used here as an internal (and non-parametric) validation procedure of the results of a multiple correspondence analysis. For supplementary variables, only partial bootstrap is possible. The partial bootstrap does not compute new MCAs: it projects bootstrap replications of the initial data as supplementary elements of the MCA. 
See references for more details.

The default parameters for ellipses assume a multivariate normal distribution drawn at level 0.95.
}

\value{a \code{ggplot2} object}

\note{
If \code{col} argument is NULL, ellipses and labels are colored according to the variables, using the default \code{ggplot2} palette. The palette can be customized using any \code{scale_color_*} function, such as \code{scale_color_brewer()}, \code{scale_color_grey()} or \code{scale_color_manual()}.
}

\references{
Lebart L. (2006). "Validation Techniques in Multiple Correspondence Analysis". In M. Greenacre et J. Blasius (eds), \emph{Multiple Correspondence Analysis and related techniques}, Chapman and Hall/CRC, p.179-196.

Lebart L. (2007). "Which bootstrap for principal axes methods?". In P. Brito et al. (eds), \emph{Selected Contributions in Data Analysis and Classification}, Springer, p.581-588.
}

\author{Nicolas Robette}

\seealso{
\code{\link{bootvalid_supvars}}, \code{\link{ggbootvalid_variables}}
}

\examples{
# specific MCA of Taste example data set
data(Taste)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA",
          "Comedy.NA", "Crime.NA", "Animation.NA", "SciFi.NA", "Love.NA", 
          "Musical.NA")
mca <- speMCA(Taste[,1:11], excl = junk)
# bootstrap validation ellipses
# for three supplementary variables
sup <- Taste[,c("Gender", "Age", "Educ")]
ggbootvalid_supvars(mca, sup)
}

\keyword{ multivariate }
\keyword{ aplot }
