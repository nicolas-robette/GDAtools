\name{tabcontrib}

\alias{tabcontrib}

\title{Table with the main contributions of categories to an axis}

\description{
Identifies the categories that contribute the most to a given dimension of a Multiple Correspondence 
Analysis and organizes these informations into a fancy table.
}

\usage{tabcontrib(resmca, dim = 1,
best = TRUE, limit = NULL,
dec = 2, shortlabs = FALSE)}

\arguments{
  \item{resmca}{object created with \code{MCA}, \code{speMCA}, \code{csMCA}, \code{wcMCA} or \code{bcMCA} function}
  \item{dim}{dimension to describe (default is 1st dimension)}
  \item{best}{if FALSE, displays all the categories; if TRUE (default), displays only categories which contribute the most (see limit argument below)}
  \item{limit}{numerical value between 0 and 100. If best = TRUE (see above), only categories with percentage of contribution higher or equal to limit are displayed. If best = TRUE and limit = NULL (default), only categories with contributions higher or equal to average are displayed.}
  \item{dec}{integer. The number of decimals for the results (default is 2)}
  \item{shortlabs}{logical. If TRUE, the data frame will have short column names, so that all columns can be displayed side by side on a laptop screen. Default is FALSE (long explicit column names).}
}

\value{
A data frame with the following contributions.: 
  \item{Variable}{names of the variables}
  \item{Category}{names of the categories}
  \item{Weight}{weights of the categories}
  \item{Quality of representation}{quality of representation (squared cosine) of the categories on the axis}
  \item{Contribution (left)}{contributions of the categories located on one side of the axis}
  \item{Contribution (right)}{contributions of the categories located on the other side of the axis}
  \item{Total contribution}{contributions summed by variable}
  \item{Cumulated contribution}{cumulated sum of the contributions}
  \item{Contribution of deviation}{for each variable, contribution of the deviation between the barycenter of the categories located on one side of the axis and the barycenter of those located on the other side}
  \item{Proportion to variable}{contribution of deviation expressed as a proportion of the contribution of the variable}
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Nicolas Robette}

\seealso{
\code{\link{dimcontrib}}, \code{\link{dimdescr}}, \code{\link{dimeta2}}, \code{\link{dimtypicality}}
}

\examples{
# specific MCA of Music example data set
data(Music)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA")
mca <- speMCA(Music[,1:5], excl = junk)
# main contributions on axis 1
tabcontrib(mca, 1)
# main contributions on axis 2
tabcontrib(mca, 2)
}

\keyword{ multivariate }
\keyword{ misc }
