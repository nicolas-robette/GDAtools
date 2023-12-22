\name{dichotomixed}

\alias{dichotomixed}

\title{Dichotomizes the factor variables in a mixed format data frame}

\description{Dichotomizes the factor variables in a data frame composed of mixed format variables, i.e. transforms the factors into an indicator matrix (also known as disjunctive table) and keeps the numerical variables.}

\usage{dichotomixed(data, out = "numeric")}

\arguments{
  \item{data}{data frame of categorical and numerical variables}
  \item{out}{character string defining the format for dichotomized variables in the output data frame. Format may be "numeric" (default) or "factor".}
}

\value{
Returns a data frame with numerical variables and dichotomized factor variables}

\author{Nicolas Robette}

\examples{
## Dichotomizes Music example data frame
data(Music)
## recodes Age as numerical, for the sake of the example
Music$Age <- as.numeric(Music$Age)
## dichotomization
dic <- dichotomixed(Music)
str(dic)
}

\keyword{ multivariate }
\keyword{ misc }
