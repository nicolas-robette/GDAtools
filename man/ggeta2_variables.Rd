\name{ggeta2_variables}
\alias{ggeta2_variables}

\title{eta-squared plot}

\description{
Plots the eta-squared (squared correlation ratios) of the active variables of a MCA.
}

\usage{
ggeta2_variables(resmca, axes = c(1,2))
}

\arguments{
  \item{resmca}{object created with \code{MCA}, \code{speMCA}, \code{csMCA}, \code{wcMCA}, \code{bcMCA}, \code{stMCA} or \code{multiMCA} function}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
}

\details{
This plot was proposed by Escofier and Pagès (2008) under the name "carré des liaisons", i.e. square of relationships, using correlation ratios to measure these relationships. Eta-squared (i.e. correlation ratio) is a measure of global association between a continuous variable and a categorical variable : it measures the share of variance of the continuous variables "explained" by the categorical variable. Here, it is used to plot the association between the active variables and the axes of the MCA cloud.
}

\value{a \code{ggplot2} object}

\references{
Escofier B. and Pagès J., 2008, \emph{Analyses factorielles simples et multiples}, Dunod.
}

\author{Nicolas Robette}

\seealso{
\code{\link{ggcloud_variables}}, \code{\link{ggadd_attractions}}
}

\examples{
data(Music)
junk <- c("FrenchPop.NA","Rap.NA","Jazz.NA","Classical.NA","Rock.NA")
mca <- speMCA(Music[,1:5], excl = junk)
ggeta2_variables(mca)
}

\keyword{ aplot }
\keyword{ multivariate }
