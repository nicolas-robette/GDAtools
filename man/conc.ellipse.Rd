\name{conc.ellipse}

\alias{conc.ellipse}

\title{Adds concentration ellipses to a cloud of individuals.}

\description{Adds inertia ellipses to the cloud of individuals of a MCA. By default, concentration ellipses are plotted.}

\usage{
conc.ellipse(resmca, var, sel = 1:nlevels(var), axes = c(1, 2),
 kappa=2,
 col = rainbow(length(sel)), pcol = rainbow(length(sel)), pcex = 0.2,
 lty = 1, lwd = 1, tcex = 1, text.lab = TRUE)}

\arguments{
  \item{resmca}{object of class \code{'MCA'}, \code{'speMCA'}, \code{'csMCA'}, \code{'multiMCA'} or \code{'stMCA'}}
  \item{var}{supplementary variable to plot}
  \item{sel}{numeric vector of indexes of the categories to plot (by default, ellipses are plotted for every categories)}
  \item{axes}{length 2 vector specifying the components to plot (default is c(1,2))}
  \item{kappa}{numeric. The kappa value (i.e. "index") of the inertia ellipses. By default, kappa = 2, which means that concentration ellipses are plotted.}
  \item{col}{vector of colors for the ellipses of plotted categories (by default, rainbow palette is used)}
  \item{pcol}{vector of colors for the points at the center of ellipses of plotted categories (by default, rainbow palette is used)}
  \item{pcex}{numerical value giving the amount by which points at the center of ellipses should be magnified (default is 0.2)}
  \item{lty}{line type for ellipses (default is 1)}
  \item{lwd}{line width for the ellipses (default is 1)}
  \item{tcex}{numerical value giving the amount by which labels at the center of ellipses should be magnified (default is 0.2)}
  \item{text.lab}{whether the labels at the center of ellipses should be displayed (default is TRUE)}
}

\details{
If kappa=2, ellipses are called "concentration" ellipses and, for a normally shaped subcloud, contain 86.47 percents of the points of the subcloud. If kappa=1, ellipses are "indicator" ellipses and contain 39.35 percents of the points of the subcloud. If kappa=1.177, ellipses are "median" ellipses and contain 50 percents of the points of the subcloud.
This function has to be used after the cloud of individuals has been drawn.
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{plot.speMCA}}, \code{\link{plot.csMCA}}, \code{\link{plot.multiMCA}}, \code{\link{plot.stMCA}}
}

\examples{
## Performs specific MCA (excluding 'NA' categories) of 'Taste' example data set,
## plots the cloud of categories
## and adds concentration ellipses for gender variable
data(Taste)
mca <- speMCA(Taste[,1:11],excl=c(3,6,9,12,15,18,21,24,27,30,33))
plot(mca,type='i')
conc.ellipse(mca,Taste$Gender)

## Draws a blue concentration ellipse for men only
plot(mca,type='i')
conc.ellipse(mca,Taste$Gender,sel=1,col='blue')
}

\keyword{aplot}
\keyword{multivariate}
\keyword{misc}
