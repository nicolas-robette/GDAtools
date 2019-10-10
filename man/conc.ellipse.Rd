\name{conc.ellipse}

\alias{conc.ellipse}

\title{Adds concentration ellipses to a correspondence analysis graph.}

\description{Adds concentration ellipses to the individuals factor map of a correspondence analysis.}

\usage{
conc.ellipse(resmca, var, sel = 1:length(levels(varb)), col = rainbow(length(sel)), 
axes = c(1, 2), cex = 0.2)}

\arguments{
  \item{resmca}{object of class \code{'MCA'}, \code{'speMCA'}, \code{'csMCA'}, \code{'multiMCA'} or \code{'stMCA'}}
  \item{var}{supplementary variable to plot}
  \item{sel}{numeric vector of indexes of the categories to plot (by default, ellipses are plotted for every categories)}
  \item{col}{vector of colors for the ellipses of plotted categories (by default, rainbow palette is used)}
  \item{axes}{length 2 vector specifying the components to plot (default is c(1,2))}
  \item{cex}{numerical value giving the amount by which ellipse contour should be magnified (default is 0.2)}
}

\details{This function has to be used after the cloud of individuals has been drawn.}

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
