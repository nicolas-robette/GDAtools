\name{quasindep}
\alias{quasindep}

\title{Quasi-correspondence analysis}

\description{
Transforms a symmetrical contingency table so that it can be used for quasi-correspondence analysis, also called correspondence analysis of incomplete contingency table.
}

\usage{
quasindep(tab, order = 3, tol = 1e-6)
}

\arguments{
  \item{tab}{a symmetric table or matrix}
  \item{order}{numeric value. Order of reconstitution of the quasi-independence data. Default is 3.}
  \item{tol}{numeric value. The tolerance threshold to be considered for convergence to null during iteration process. Default is 1e-6.}
}

\details{
In order to carry out a "quasi-correspondence analysis", also called "correspondence analysis of incomplete table", the principle is to stop analyzing the differences between the observed data and the situation of independence between the variable in rows and the variable in columns, as it is the case in the classical correspondence analysis, and to consider the differences between the data and a situation of quasi-independence, i.e. independence for some cells of the table only. In the most common situation, it is therefore a matter of applying the independence hypothesis to the off-diagonal cells only and replacing the diagonal with values that do not influence the analysis. Such values are obtained in an iterative way by replacing the numbers of the cells of the diagonal by their third order reconstruction, then by recalculating the correspondence analysis until convergence is reached. The algorithm used is developed in van der Heijden (1992: 11-12).
}

\note{
This function is adapted from Milan Bouchet-Valat's script in the supplementary material of his article indicated in the reference section.
}

\value{
An object of the same class and dimensions as \code{tab} : the quasi-independence data to be analyzed with Correspondence Analysis.
}

\references{
De Leeuw J et van der Heijden PGM (1985) \emph{Quasi-Correspondence Analysis}. Leiden: University
of Leiden.

Van der Heijden PGM (1992) Three Approaches to Study the Departure from Quasi-independence. \emph{Statistica Applicata} 4: 465-80.

Bouchet-Valat M (2015) L'analyse statistique des tables de contingence carrées - L'homogamie socioprofessionnelle en
France - I, L'analyse des correspondances \emph{Bulletin de Méthodologie Sociologique} 125: 65–88. <doi:10.1177/0759106314555655>
}

\examples{
\dontrun{
tab <- matrix(c(165,49,70,100,48,223,
                6,201,226,212,90,216,
                4,96,446,214,72,77,
                5,84,305,317,126,188,
                3,52,151,190,110,189,
                17,234,310,601,309,1222),
                nrow = 6, ncol = 6, byrow = TRUE)
newtab <- quasindep(tab)
}
}

\keyword{ multivariate }
\keyword{ misc }
