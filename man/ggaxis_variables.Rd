\name{ggaxis_variables}
\alias{ggaxis_variables}

\title{Plots MCA variables on a single axis}

\description{
Plots variables on a single axis of a Multiple Correspondence Analysis, using ggplot2. Variables can be active or supplementary.
}

\usage{
ggaxis_variables(resmca, var = NULL, axis = 1, prop = NULL,
underline = FALSE, color = "black", palette = "khroma::bright")
}

\arguments{
  \item{resmca}{object of class \code{MCA}, \code{speMCA}, \code{csMCA}, \code{stMCA} or \code{multiMCA}.}
  \item{var}{If NULL (default), all the active variables of the MCA are plotted. If a character string, the named active variable of the MCA is plotted. If a factor, plotted as a supplementary variable.}
  \item{axis}{numeric value. The MCA axis to plot. Default is 1.}
  \item{prop}{If NULL (default), the size of the labels is constant. If "freq", the size is proportional to the weights of categories. If "ctr", it's propotional to the contributions of categories (only used for active variables). If "cos2", it's proportional to the squared cosines of the categories. If "pval", it's proportional to 1 minus the p-values of typicality tests (only used for supplementary variables). If "cor", it's proportional to the point biserial correlation of the categories (only used for supplementary variables).}
  \item{underline}{logical. If TRUE, the labels of the categories with contributions above average are underlined. Default is FALSE. Only used for active variables.}
  \item{color}{character string. Color name, used if a single variable is plotted or if var is NULL.}
  \item{palette}{character string. Palette name, in the form packagename::palettename (cf \code{paletteer} package). If NULL, color argument is used. Default is "khroma::bright", a colorblind friendly palette.}
}

\value{a ggplot object}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{ggcloud_variables}}, \code{\link{ggadd_supvar}}, \code{\link{ggadd_ellipses}}, \code{\link{ggadd_corr}}, \code{\link{ggadd_interaction}}, \code{\link{ggadd_density}}
}

\examples{
## Performs a specific MCA on 'Taste' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## and then draws the categories on the first axis.
data(Taste)
resmca <- speMCA(Taste[,1:11], excl=c(3,6,9,12,15,18,21,24,27,30,33))
ggaxis_variables(resmca)
ggaxis_variables(resmca, prop = "freq", underline = TRUE, color = "black")
ggaxis_variables(resmca, var = "Classical", axis = 1, prop = "ctr", underline = TRUE)
ggaxis_variables(resmca, var = Taste$Educ, axis = 1, prop = "pval")
}

\keyword{ aplot }
\keyword{ multivariate }
