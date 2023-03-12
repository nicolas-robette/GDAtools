\name{ggsmoothed_supvar}
\alias{ggsmoothed_supvar}

\title{Plots the density a supplementary variable in a MCA space}

\description{
Plots the density of a supplementary variable in a MCA space, using a grid, smoothing and interpolation (via inverse distance weighting.)
}

\usage{
ggsmoothed_supvar(resmca, var, cat, axes = c(1,2), 
                  center = FALSE, scale = FALSE,
                  nc = c(20, 20), power = 2,
                  limits = NULL, pal = "RdBu")
}

\arguments{
  \item{resmca}{object of class \code{PCA}, \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}.}
  \item{var}{factor or numeric vector. The supplementary variable to be plotted.}
  \item{cat}{character string. If \code{var} is a factor, the name of the level of the supplementary variable to be plotted.}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{center}{logical. Whether the supplementary variable should be centered or not. Default is FALSE.}
  \item{scale}{logical. Whether the supplementary variable should be scaled to unit variance or not. Default is FALSE.}
  \item{nc}{integer vector of length 2. Number of grid cells in x and y direction (columns, rows).}
  \item{power}{numerical value. The power to use in weight calculation for inverse distance weighting. Default is 2.}
  \item{limits}{numerical vector of length 2. Lower and upper limit of the scale for the supplementary variable.}
  \item{pal}{name of a palette from the \code{RcolorBrewer} package. Default is "RdBu".}
}

\details{
The construction of the plot takes place in several steps. First, the two-dimensional factorial space is cut into a grid of hexagonal cells. Then, for each cell, the average value of the supplementary variable is calculated for the observations located in that cell (if the variable is numerical), or the proportion of observations belonging to the category studied (if the variable is categorical). The results are interpolated and smoothed to make the plot easier to read, using the \emph{inverse distance weighting} technique, which is very common in spatial analysis.

The supplementary variable can be centered beforehand, to represent deviations from the mean (for a numerical variable) or from the mean proportion (for a categorical variable). It can also be scaled to measure deviations in numbers of standard deviations, which can be useful for comparing the results of several supplementary variables.
}

\value{a ggplot object}

\references{
Shepard, Donald (1968). "A two-dimensional interpolation function for irregularly-spaced data". \emph{Proceedings of the 1968 ACM National Conference}. pp. 517–524. doi:10.1145/800186.810616
}

\author{Nicolas Robette}

\seealso{
\code{\link{ggadd_supvar}}, \code{\link{ggadd_ellipses}}, \code{\link{ggadd_kellipses}}
}

\examples{
data(Taste)
res <- speMCA(Taste[,1:11], excl=c(3,6,9,12,15,18,21,24,27,30,33))
ggsmoothed_supvar(res, Taste$Educ, "High")
ggsmoothed_supvar(res, as.numeric(Taste$Age), center = TRUE, scale = TRUE)
}

\keyword{ aplot }
\keyword{ multivariate }
