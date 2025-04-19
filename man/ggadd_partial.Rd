\name{ggadd_partial}
\alias{ggadd_partial}

\title{Main and partial effect of a supplementary variable}

\description{
Adds the main and partial effects of a supplmentary variable to a MCA cloud of individuals, with one or more supplementary partialled out
}

\usage{
ggadd_partial(p, resmca, var, control, sel = 1:nlevels(var),
axes = c(1,2), col = "black", textsize = 4, lines = TRUE, dashes = TRUE, 
legend = "right", force = 1, max.overlaps = Inf)
}

\arguments{
  \item{p}{\code{ggplot2} object with the cloud of variables or the cloud of individuals}
  \item{resmca}{object created with \code{MCA}, \code{speMCA}, \code{csMCA}, \code{wcMCA}, \code{bcMCA}, \code{stMCA} or \code{multiMCA} function}
  \item{var}{factor. The categorical supplementary variable.}
  \item{control}{data frame of supplementary variables to be partialled out (i.e. control variables)}
  \item{sel}{numeric vector of indexes of the categories of the supplementary variable. By default, every categories are used.}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{col}{the color for the labels and lines. Default is "black".}
  \item{textsize}{size of the labels of categories. Default is 4.}
  \item{lines}{logical. Whether to add colored lines between the points of the categories of v1. Default is TRUE.}  
  \item{dashes}{logical. Whether to add gray dashed lines between the points of the categories of v2. Default is TRUE.}
  \item{legend}{the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector). Default is right.}
  \item{force}{force of repulsion between overlapping text labels. Defaults to 1. If 0, labels are not repelled at all.}
  \item{max.overlaps}{exclude text labels that overlap too many things. Defaults to Inf, which means no labels are excluded.} 
}

\value{a \code{ggplot2} object}

\note{
Coordinates of the partial effect of the supplementary variables are derives from linear regression, with individual coordinates as dependent variable, and the supplementary and control variables as independent variables.
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{ggcloud_variables}}, \code{\link{ggadd_supvar}}, \code{\link{ggadd_supvars}}, \code{\link{ggadd_interaction}}
}

\examples{
# specific MCA of Taste example data set
data(Taste)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA",
          "Comedy.NA", "Crime.NA", "Animation.NA", "SciFi.NA", "Love.NA", 
          "Musical.NA")
mca <- speMCA(Taste[,1:11], excl = junk)
# effect of education,
# with age partialled out (partial effect) or not (main effect)
p <- ggcloud_indiv(mca, col = "grey95")
ggadd_partial(p, mca, Taste$Educ, Taste$Age)
}

\keyword{ aplot }
\keyword{ multivariate }
