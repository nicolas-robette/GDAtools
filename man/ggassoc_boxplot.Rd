\name{ggassoc_boxplot}

\alias{ggassoc_boxplot}

\title{Boxplots with violins}

\description{
Displays of boxplot and combines it with a violin plot, using ggplot2.
}

\usage{
ggassoc_boxplot(data, mapping, axes.labs=TRUE, ticks.labs=TRUE,
text.size=3, box=TRUE, notch=FALSE, violin=TRUE)
}

\arguments{
  \item{data}{dataset to use for plot}
  \item{mapping}{aesthetic being used. It must specify x and y.}
  \item{axes.labs}{Whether to display the labels of the axes, i.e. the names of x and y. Default is TRUE.}
  \item{ticks.labs}{Whether to display the labels of the categories of x and y. Default is TRUE.}
  \item{text.size}{Size of the association measure. If NULL, the text is not added to the plot.}
  \item{box}{Whether to draw boxplots. Default is TRUE.}
  \item{notch}{If FALSE (default) make a standard box plot. If TRUE, make a notched box plot. Notches are used to compare groups; if the notches of two boxes do not overlap, this suggests that the medians are significantly different.}
  \item{violin}{Whether to draw a violin plot. Default is TRUE.}
}

\details{
Eta-squared measure of global association between x and y is displayed in upper-left corner of the plot.

This function can be used as a high-level plot with \code{ggduo} and \code{ggpairs} functions of the \code{GGally} package. 
}

\value{
a ggplot object
}

\author{Nicolas Robette}

\seealso{
 \code{\link{assoc.catcont}}, \code{\link{condesc}}, \code{\link{assoc.yx}},
 \code{\link{darma}}, \code{\link[GGally]{ggpairs}}
}

\examples{
data(decathlon, package="FactoMineR")
ggassoc_boxplot(decathlon, mapping = ggplot2::aes(x = Javeline, y = Competition))
}

\keyword{ multivariate }
\keyword{ aplot }
