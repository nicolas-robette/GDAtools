\name{barplot_contrib}

\alias{barplot_contrib}

\title{Bar plot of contributions}

\description{From MCA results, plots contributions to the axes.}

\usage{

barplot_contrib(resmca, dim = 1, which = "var",
  sort = FALSE, col = "tomato4", repel = FALSE)
  }

\arguments{
  \item{resmca}{object created with \code{MCA}, \code{speMCA}, \code{csMCA}, \code{wcMCA}, \code{bcMCA}, \code{stMCA} or \code{multiMCA} function}
  \item{dim}{the dimension to use. Default is 1.}
  \item{which}{If \code{resmca} is of class \code{MCA}, \code{speMCA}, \code{csMCA} or \code{PCA}, should be \code{"var"} to plot contributions of variables or \code{"ind"} to plot contributions of individuals. If \code{resmca} is of class \code{CA}, should be \code{"row"} to plot contributions of rows or \code{"col"} to plot contributions of columns. Default is \code{"var"}.}
  \item{sort}{logical. If \code{TRUE}, bars are sorted by decreasing VIPs. Default is \code{FALSE}.}
  \item{col}{color of the bars}
  \item{repel}{logical. If \code{TRUE}, the names of the variables are repelled with \code{geom_text_repel}. Default is \code{FALSE}}
}

\details{
The contributions are multiplied by the sign of the coordinates, so that the plot shows on which side of the axis they contribute, which makes the interpretation easier.
}

\value{
a \code{ggplot2} object
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{speMCA}}, \code{\link{tabcontrib}}
}

\examples{
# specific MCA on the Music example data set
data(Music)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA")
mca <- speMCA(Music[,1:5], excl = junk)
# contributions of categories
barplot_contrib(mca)
}

\keyword{ multivariate }
\keyword{ misc }
