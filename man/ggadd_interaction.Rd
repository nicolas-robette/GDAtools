\name{ggadd_interaction}
\alias{ggadd_interaction}

\title{Plot of interactions between two categorical supplementary variables}

\description{
Adds the interactions between two categorical supplementary variables to a MCA cloud of variables
}

\usage{
ggadd_interaction(p, resmca, v1, v2, sel1 = 1:nlevels(v1), sel2 = 1:nlevels(v2),
axes = c(1,2), textsize = 5, legend = "right")
}

\arguments{
  \item{p}{\code{ggplot2} object with the cloud of variables}
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}.}
  \item{v1}{Factor. The first categorical supplementary variable.}
  \item{v2}{Factor. The second categorical supplementary variable.}
  \item{sel1}{Numeric vector of indexes of the categories of the first supplementary variable to be used in interaction. By default, every categories are used.}
  \item{sel2}{Numeric vector of indexes of the categories of the second supplementary variable to be used in interaction. By default, every categories are used.}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{textsize}{Size of the labels of categories. Default is 5.}
  \item{legend}{the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector). Default is right.}
}

\value{a \code{ggplot2} object}

\note{
Lines and labels are colored according to the variables, using the default \code{ggplot2} palette. The palette can be customized using any \code{scale_color_*} function, such as \code{scale_color_brewer()}, \code{scale_color_grey()} or \code{scale_color_manual()}.
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
