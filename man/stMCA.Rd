\name{stMCA}

\alias{stMCA}

\title{Performs a 'standardized' MCA}

\description{Performs a 'standardized' Multiple Correspondence Analysis, i.e it takes MCA results and forces all the dimensions to 
be orthogonal to a supplementary 'control' variable.}

\usage{stMCA(resmca, control)}

\arguments{
  \item{resmca}{an object of class \code{'MCA'}, \code{'speMCA'}, \code{'csMCA'} or \code{'multiMCA'}}
  \item{control}{a list of 'control' variables}
}

\details{
Standardized MCA unfolds in several steps. First, for each dimension of an input MCA, individual coordinates are used as 
dependent variable in a linear regression model and the 'control' variable is included as covariate in the same model. The 
residuals from every models are retained and bound together. The resulting data frame is composed of continuous variables and 
its number of columns is equal to the number of dimensions in the input MCA. Lastly, this data frame is used as input in 
a Principal Component Analysis.
}

\value{
Returns an object of class "stMCA". This object will be similar to \code{resmca} argument, still it does not comprehend modified rates, 
categories contributions and variables contributions.
}

\references{
Robette, Bry and Roueff, 2014, "Un dialogue de sourds dans le theatre statistique? Analyse geometrique des 
donnees et effets de structure", [http://nicolas.robette.free.fr/publis.html], \emph{forthcoming}.
}

\author{Nicolas Robette}

\seealso{
\code{\link{plot.stMCA}}, \code{\link{MCA}}, \code{\link{speMCA}}, \code{\link{csMCA}}, \code{\link{multiMCA}}, \code{\link{PCA}}
}

\examples{
## Performs a specific MCA on 'Music' example data set
## ignoring every 'NA' (i.e. 'not available') categories,
## and then performs a 'standardized' MCA controlling for age.
data(Music)
mca <- speMCA(Music[,1:5],excl=c(3,6,9,12,15))
plot(mca)
textvarsup(mca,Music$Age,col='red')
stmca <- stMCA(mca,control=list(Music$Age))
plot(stmca)
textvarsup(stmca,Music$Age,col='red')
}

\keyword{ multivariate }
