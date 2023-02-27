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
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}.}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
}

\details{
Eta-squared (i.e. squared correlation ratio) is a measure of global association between a continuous variable and a categorical variable : it measures the share of variance of the continuous variables "explained" by the categorical variable. Here, it is used to plot the association between the active variables and the axes of the MCA cloud.
}

\value{a ggplot object}

\references{
Escofier B. and Pagès J., \emph{Analyses factorielles simples et multiples}, Dunod (2008).
}

\author{Nicolas Robette}

\examples{
data(Music)
mca <- speMCA(Music[,1:5],excl=c("FrenchPop.NA","Rap.NA","Jazz.NA","Classical.NA","Rock.NA"))
ggeta2_variables(mca)
}

\keyword{ aplot }
\keyword{ multivariate }
