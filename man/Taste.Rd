\name{Taste}

\alias{Taste}

\docType{data}

\title{Taste (data)}

\description{
The data concerns tastes for music and movies of a set of 2000 individuals. It contains 5 variables of likes for music genres 
(french pop, rap, rock, jazz and classical), 6 variables of likes for movie genres (comedy, crime, 
animation, science fiction, love, musical) and 2 additional variables (gender and age).
}

\usage{data(Taste)}

\format{
  A data frame with 500 observations and the following 13 variables:
  \describe{
    \item{\code{FrenchPop}}{is a factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Rap}}{is a factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Rock}}{is a factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Jazz}}{is a factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Classical}}{is a factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Comedy}}{is a factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Crime}}{is a factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Animation}}{is a factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{SciFi}}{is a factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Love}}{is a factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Musical}}{is a factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Gender}}{is a factor with levels \code{Men}, \code{Women}}
    \item{\code{Age}}{is a factor with levels \code{15-24}, \code{25-49}, \code{50+}}
    \item{\code{Educ}}{is a factor with levels \code{none}, \code{low}, \code{medium}, \code{high}}
  }
}

\details{'NA' stands for 'not available'}

\examples{
data(Taste)
str(Taste)
}

\keyword{datasets}
