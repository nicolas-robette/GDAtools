\name{nsca.biplot}
\alias{nsca.biplot}

\title{Biplot for Nonsymmetric Correspondence Analysis}

\description{
Biplot for Nonsymmetric correspondence analysis, for analysing contingency tables with a dependence structure
}

\usage{
nsca.biplot(nsca, axes = c(1,2))
}

\arguments{
  \item{nsca}{an object of class \code{CA} created by \code{nsCA()} function}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
}

\details{
The biplots of an NSCA reflect the dependency structure of the contingency table and thus should not be interpreted as the planes of a standard CA. A first principle is that the graph displays the centred row profiles. A second principle is that the relationships between rows and columns are contained in their inner products : the rows are depicted as vectors, also called biplot axes, and the columns are projected on these vectors. If some columns have projections on the row vector far away from the origin, then the row has a comparatively large increase in predictability, and its profile deviates considerably from the marginal one, especially for that column.

For more detailed interpretational guidelines, see Kroonenberg and Lombardo (1999, pp.377-378).
}

\value{a \code{ggplot2} object}

\references{
Kroonenberg P.M. and Lombardo R., 1999, "Nonsymmetric Correspondence Analysis: A Tool for Analysing Contingency Tables with a Dependence Structure", \emph{Multivariate Behavioral Research}, 34 (3), 367-396.
}

\author{Nicolas Robette}

\seealso{
\code{\link{nsCA}}
}

\examples{
data(Music)
# The combination of Gender and Age is the predictor variable
# "Focused" listening to music is the response variable
tab <- with(Music, table(interaction(Gender, Age), OnlyMus))
nsca <- nsCA(tab)
nsca.biplot(nsca)
# Goodman and Kruskal tau
nsca$GK.tau
}

\keyword{ aplot }
\keyword{ multivariate }
