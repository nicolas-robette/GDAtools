\name{Taste}

\alias{Taste}

\docType{data}

\title{Taste (data)}

\description{
The data concerns tastes for music and movies of a set of 2000 individuals. It contains 5 variables of likes for music genres (french pop, rap, rock, jazz and classical), 6 variables of likes for movie genres (comedy, crime, animation, science fiction, love, musical) and 2 additional variables (gender and age).
}

\usage{data(Taste)}

\format{
  A data frame with 2000 observations and the following 13 variables:
  \describe{
    \item{\code{FrenchPop}}{factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Rap}}{factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Rock}}{factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Jazz}}{factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Classical}}{factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Comedy}}{factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Crime}}{factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Animation}}{factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{SciFi}}{factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Love}}{factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Musical}}{factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Gender}}{factor with levels \code{Men}, \code{Women}}
    \item{\code{Age}}{factor with levels \code{15-24}, \code{25-49}, \code{50+}}
    \item{\code{Educ}}{factor with levels \code{none}, \code{low}, \code{medium}, \code{high}}
  }
}

\details{\code{NA} stands for "not available"}

\examples{
data(Taste)
str(Taste)
}

\keyword{datasets}
