\name{ggadd_ellipses}
\alias{ggadd_ellipses}

\title{Confidence ellipses}

\description{
Adds confidence ellipses for a categorical variable to a MCA cloud of individuals
}

\usage{
ggadd_ellipses(p, resmca, var, sel = 1:nlevels(var), axes = c(1,2),
level = 0.05, label = TRUE, label.size = 3, size = 0.5, points = TRUE,
legend = "right")
}

\arguments{
  \item{p}{\code{ggplot2} object with the cloud of individuals}
  \item{resmca}{object created with \code{MCA}, \code{speMCA}, \code{csMCA}, \code{wcMCA}, \code{bcMCA}, \code{stMCA} or \code{multiMCA} function}
  \item{var}{Factor. The categorical variable used to plot ellipses.}
  \item{sel}{numeric vector of indexes of the categories to plot (by default, ellipses are plotted for every categories)}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{level}{The level at which to draw an ellipse (see \code{stat_ellipse}). Default is 0.05, which means 95 percents confidence ellipses are plotted.}
  \item{label}{Logical. Should the labels of the categories be plotted at the center of ellipses ? Default is TRUE.}
  \item{label.size}{Size of the labels of the categories at the center of ellipses. Default is 3.}
  \item{size}{Size of the lines of the ellipses. Default is 0.5.}
  \item{points}{If TRUE (default), the points are coloured according to their subcloud.}
  \item{legend}{the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector). Default is right.}
}

\value{a \code{ggplot2} object}

\details{
A confidence ellipse aims at measuring how the "true" mean point of a category differs from its observed mean point. This is achieved by constructing a confidence zone around the observed mean point. If we choose a conventional level alpha (e.g. 0.05), a (1 - alpha) (e.g. 95 percents) confidence zone is defined as the set of possible mean points that are not significantly different from the observed mean point.
}

\note{
Ellipses are colored according to the categories of the variable, using the default \code{ggplot2} palette. The palette can be customized using any \code{scale_color_*} function, such as \code{scale_color_brewer()}, \code{scale_color_grey()} or \code{scale_color_manual()}.
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{ggcloud_indiv}}, \code{\link{ggadd_supvar}}, \code{\link{ggadd_supvars}}, \code{\link{ggadd_kellipses}}, \code{\link{ggadd_density}}, \code{\link{ggadd_interaction}}, \code{\link{ggsmoothed_supvar}}, \code{\link{ggadd_chulls}}, \code{\link{ggadd_corr}}
}

\examples{
# specific MCA of Music example data set
data(Music)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA")
mca <- speMCA(Music[,1:5], excl = junk)
# confidence ellipses for Age
p <- ggcloud_indiv(mca, col = "lightgrey")
ggadd_ellipses(p, mca, Music$Age)
}

\keyword{ aplot }
\keyword{ multivariate }
