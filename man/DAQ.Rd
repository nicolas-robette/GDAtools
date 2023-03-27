\name{DAQ}
\alias{DAQ}

\title{Discriminant Analysis of Qualitative Variables}

\description{Descriptive discriminant analysis (aka "Analyse Factorielle Discriminante" for the French school of multivariate data analysis) with qualitative variables.}

\usage{
DAQ(data, class, excl = NULL, row.w = NULL,
    type = "FR", select = TRUE)
}

\arguments{
  \item{data}{data frame with only categorical variables}
  \item{class}{factor specifying the class}
  \item{excl}{numeric vector indicating the indexes of the "junk" categories (default is NULL). See \code{\link{getindexcat}} or use \code{\link{ijunk}} interactive function to identify these indexes. It may also be a character vector of junk categories, specified in the form "namevariable.namecategory" (for instance "gender.male").}
  \item{row.w}{numeric vector of row weights. If NULL (default), a vector of 1 for uniform row weights is used.}
  \item{type}{character string. If "FR" (default), the inverse of the total covariance matrix is used as metric. If "GB", it is the inverse of the within-class covariance matrix (Mahalanobis metric), which makes the results equivalent to linear discriminant analysis as implemented in \code{lda} function in \code{MASS} package.}  
  \item{select}{logical. If TRUE (default), only a selection of components of the MCA are used for the discriminant analysis step. The selected components are those corresponding to eigenvalues higher of equal to 1/Q, with Q the number of variables in \code{data}. If FALSE, all components are used.}
}

\details{
This approach is also known as "disqual" and was developed by G. Saporta (see references). It consists in two steps : 
1. Multiple Correspondence Analysis of the data
2. Discriminant analysis of the components from the MCA

The results are the same with \code{type} "FR" or "GB", only the eigenvalues vary. With \code{type="FR"}, these eigenvalues vary between 0 and 1 and can be interpreted as "discriminant power".
}

\value{
An object of class \code{PCA} from \code{FactoMineR} package, with \code{class} as qualitative supplementary variable and the disjunctive table of \code{data} as quantitative supplementary variables, and two additional items :
\item{cor_ratio}{correlation ratios between \code{class} and the discriminant factors}
\item{mca}{an object of class \code{speMCA} with the results of the MCA of the first step}
}

\references{
Bry X., 1996, \emph{Analyses factorielles multiples}, Economica.

Lebart L., Morineau A. et Warwick K., 1984, \emph{Multivariate Descriptive Statistical Analysis}, John Wiley and sons, New-York.)

Saporta G., 1977, "Une méthode et un programme d'analyse discriminante sur variables qualitatives", \emph{Premières Journées Internationales, Analyses des données et informatiques}, INRIA, Rocquencourt.

Saporta G., 2006, \emph{Probabilités, analyses des données et statistique}, Editions Technip.
}

\author{Nicolas Robette}

\seealso{
\code{\link{DA}}, \code{\link{bcMCA}}, \code{\link{MCAiv}}, \code{\link{speMCA}}
}

\examples{
library(FactoMineR)
data(tea)
res <- DAQ(tea[,1:18], tea$SPC)
# plot of observations colored by class
plot(res, choix = "ind", invisible = "quali", 
     label = "quali", habillage = res$call$quali.sup$numero)
# plot of class categories
plot(res, choix = "ind", invisible = "ind", col.quali = "black")
# plot of the variables in data
plot(res, choix = "var", invisible = "var")
# plot of the components of the MCA
plot(res, choix = "varcor", invisible = "quanti.sup")
}

\keyword{ multivariate }
