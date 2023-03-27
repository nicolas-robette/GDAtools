\name{medoids}

\alias{medoids}

\title{Medoids of clusters}

\description{Computes the medoids of a cluster solution.}

\usage{medoids(D, cl)}

\arguments{
  \item{D}{square distance matrix (n rows * n columns, i.e. n individuals) or \code{dist} object}
  \item{cl}{vector with the clustering solution (its length should be n)}
}

\details{
A medoid is a representative object of a cluster whose average dissimilarity to all the objects in the cluster is minimal. Medoids are always members of the data set (contrary to means or centroids).
}

\value{Returns a numeric vector with the indexes of medoids.}

\references{
Kaufman, L. and Rousseeuw, P.J. (1990). \emph{Finding Groups in Data: An Introduction to Cluster Analysis}. Wiley, New York.

Anja Struyf, Mia Hubert & Peter J. Rousseeuw (1996). "Clustering in an Object-Oriented Environment". \emph{Journal of Statistical Software}.
}

\author{Nicolas Robette}

\seealso{
\code{\link{dist}}, \code{\link{cluster}}, \code{\link{hclust}}, \code{\link{cutree}} , \code{\link{pam}} 
}

\examples{
# hierarchical clustering of the Music example data set, 
# partition into 3 groups
# and then computation of the medoids.
data(Music)
temp <- dichotom(Music[,1:5])
d <- dist(temp)
clus <- cutree(hclust(d), 3)
medoids(d, clus)
}

\keyword{ multivariate }
\keyword{ misc }
