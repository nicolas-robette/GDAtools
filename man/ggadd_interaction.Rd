\name{ggadd_interaction}
\alias{ggadd_interaction}

\title{Plot of interactions between two categorical supplementary variables}

\description{
Adds the interactions between two categorical supplementary variables to a MCA cloud of variables
}

\usage{
ggadd_interaction(p, resmca, v1, v2, sel1 = 1:nlevels(v1), sel2 = 1:nlevels(v2),
axes = c(1,2), cloud = "v", textsize = 5, lines = TRUE, dashes = TRUE, 
legend = "none", force = 1, max.overlaps = Inf)
}

\arguments{
  \item{p}{\code{ggplot2} object with the cloud of variables or the cloud of individuals}
  \item{resmca}{object created with \code{MCA}, \code{speMCA}, \code{csMCA}, \code{wcMCA}, \code{bcMCA}, \code{stMCA} or \code{multiMCA} function}
  \item{v1}{factor. The first categorical supplementary variable.}
  \item{v2}{factor. The second categorical supplementary variable.}
  \item{sel1}{numeric vector of indexes of the categories of the first supplementary variable to be used in interaction. By default, every categories are used.}
  \item{sel2}{numeric vector of indexes of the categories of the second supplementary variable to be used in interaction. By default, every categories are used.}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{cloud}{if "v" (default), the categories are plotted in the cloud of variables. If "i", the categories are plotted in the cloud of individuals.}
  \item{textsize}{size of the labels of categories. Default is 5.}
  \item{lines}{logical. Whether to add colored lines between the points of the categories of v1. Default is TRUE.}  
  \item{dashes}{logical. Whether to add gray dashed lines between the points of the categories of v2. Default is TRUE.}
  \item{legend}{the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector). Default is none.}
  \item{force}{force of repulsion between overlapping text labels. Defaults to 1. If 0, labels are not repelled at all.}
  \item{max.overlaps}{exclude text labels that overlap too many things. Defaults to Inf, which means no labels are excluded.} 
}

\value{a \code{ggplot2} object}

\note{
Lines and labels are colored according to the first variable, using the default \code{ggplot2} palette. The palette can be customized using any \code{scale_color_*} function, such as \code{scale_color_brewer()}, \code{scale_color_grey()} or \code{scale_color_manual()}.
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{ggcloud_variables}}, \code{\link{ggadd_supvar}}, \code{\link{ggadd_supvars}}, \code{\link{ggadd_kellipses}}, \code{\link{ggadd_ellipses}}, \code{\link{ggadd_corr}}, \code{\link{ggsmoothed_supvar}}, \code{\link{ggadd_chulls}}, \code{\link{ggadd_density}}
}

\examples{
# specific MCA of Taste example data set
data(Taste)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA",
          "Comedy.NA", "Crime.NA", "Animation.NA", "SciFi.NA", "Love.NA", 
          "Musical.NA")
mca <- speMCA(Taste[,1:11], excl = junk)
# interaction between Gender and Age
p <- ggcloud_variables(mca, col = "lightgrey", shapes = FALSE)
ggadd_interaction(p, mca, Taste$Gender, Taste$Age)
}

\keyword{ aplot }
\keyword{ multivariate }
