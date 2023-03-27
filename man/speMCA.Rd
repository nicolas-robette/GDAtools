\name{speMCA}

\alias{speMCA}

\title{specific MCA}

\description{Performs a specific Multiple Correspondence Analysis, i.e. a variant of MCA that allows to treat undesirable categories as passive categories.}

\usage{
speMCA(data, excl = NULL, ncp = 5, row.w = NULL)
}

\arguments{
  \item{data}{data frame with n rows (individuals) and p columns (categorical variables)}
  \item{excl}{numeric vector indicating the indexes of the "junk" categories (default is NULL). See \code{\link{getindexcat}} or use \code{\link{ijunk}} interactive function to identify these indexes. It may also be a character vector of junk categories, specified in the form "namevariable.namecategory" (for instance "gender.male").}
  \item{ncp}{number of dimensions kept in the results (default is 5)}
  \item{row.w}{an optional numeric vector of row weights. If NULL (default), a vector of 1 for uniform row weights)}
}

\details{
Undesirable categories may be of several kinds: infrequent categories (say, <5 percents), heterogeneous categories (e.g. "others") or uninterpretable categories (e.g. "not available"). In these cases, specific MCA may be useful to ignore these categories for the determination of distances between individuals (see references).
}

\value{
Returns an object of class \code{speMCA}, i.e. a list including:
  \item{eig}{a list of vectors containing all the eigenvalues, the percentage of variance, the cumulative percentage of variance, the modified rates and the cumulative modified rates}
  \item{call}{a list with informations about input data}
  \item{ind}{a list of matrices containing the results for the individuals (coordinates, contributions, squared cosines and total distances)}
  \item{var}{a list of matrices containing all the results for the categories and variables (weights, coordinates, squared cosines, categories contributions to axes and cloud, test values (v.test), squared correlation ratio (eta2), variable contributions to axes and cloud, total distances}
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{getindexcat}}, \code{\link{ijunk}}, \code{\link{plot.speMCA}}, \code{\link{ggcloud_indiv}}, \code{\link{ggcloud_variables}}, \code{\link{csMCA}}
}

\examples{
# specific MCA of Music example data set
data(Music)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA")
mca <- speMCA(Music[,1:5], excl = junk)
# This is equivalent to :
mca <- speMCA(Music[,1:5], excl = c(3,6,9,12,15))
}

\keyword{ multivariate }
