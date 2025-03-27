\name{ggadd_kellipses}
\alias{ggadd_kellipses}

\title{Concentration ellipses and k-inertia ellipses}

\description{
Adds concentration ellipses and other kinds of k-inertia ellipses for a categorical variable to a MCA cloud of individuals.
}

\usage{
ggadd_kellipses(p, resmca, var, sel = 1:nlevels(var), axes = c(1,2),
kappa = 2, label = TRUE, label.size = 3, size = 0.5, points = TRUE,
legend = "right")
}

\arguments{
  \item{p}{\code{ggplot2} object with the cloud of individuals}
  \item{resmca}{object created with \code{MCA}, \code{speMCA}, \code{csMCA}, \code{wcMCA}, \code{bcMCA}, \code{stMCA} or \code{multiMCA} function}
  \item{var}{Factor. The categorical variable used to plot ellipses.}
  \item{sel}{numeric vector of indexes of the categories to plot (by default, ellipses are plotted for every categories)}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{kappa}{numeric. The kappa value (i.e. "index") of the inertia ellipses. By default, kappa = 2, which means that concentration ellipses are plotted.}
  \item{label}{Logical. Should the labels of the categories be plotted at the center of ellipses ? Default is TRUE.}
  \item{label.size}{Size of the labels of the categories at the center of ellipses. Default is 3.}  
  \item{size}{Size of the lines of the ellipses. Default is 0.5.}
  \item{points}{If TRUE (default), the points are coloured according to their subcloud.}
  \item{legend}{the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector). Default is right.}
}

\value{a \code{ggplot2} object}

\details{
If kappa=2, ellipses are called "concentration" ellipses and, for a normally shaped subcloud, contain 86.47 percents of the points of the subcloud. If kappa=1, ellipses are "indicator" ellipses and contain 39.35 percents of the points of the subcloud. If kappa=1.177, ellipses are "median" ellipses and contain 50 percents of the points of the subcloud.
This function has to be used after the cloud of individuals has been drawn.
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
\code{\link{ggcloud_indiv}}, \code{\link{ggadd_supvar}}, \code{\link{ggadd_supvars}}, \code{\link{ggadd_ellipses}}, \code{\link{ggadd_density}}, \code{\link{ggadd_interaction}}, \code{\link{ggsmoothed_supvar}}, \code{\link{ggadd_chulls}}, \code{\link{ggadd_corr}}
}

\examples{
# specific MCA of Music example data set
data(Music)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA")
mca <- speMCA(Music[,1:5], excl = junk)
# concentration ellipses for Age
p <- ggcloud_indiv(mca, col = "lightgrey")
ggadd_ellipses(p, mca, Music$Age)
}

\keyword{ aplot }
\keyword{ multivariate }
