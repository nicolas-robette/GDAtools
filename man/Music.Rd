\name{Music}

\alias{Music}

\docType{data}

\title{Music (data)}

\description{
The data concerns tastes for music of a set of 500 individuals. It contains 5 variables of likes for music genres
 (french pop, rap, rock, jazz and classical), 2 about music listening and 2 additional variables (gender and age).
}

\usage{data(Music)}

\format{
  A data frame with 500 observations and the following 7 variables:
  \describe{
    \item{\code{FrenchPop}}{is a factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Rap}}{is a factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Rock}}{is a factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Jazz}}{is a factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Classical}}{is a factor with levels \code{No}, \code{Yes}, \code{NA}}
    \item{\code{Gender}}{is a factor with levels \code{Men}, \code{Women}}
    \item{\code{Age}}{is a factor with levels \code{15-24}, \code{25-49}, \code{50+}}
    \item{\code{OnlyMus}}{is a factor with levels \code{Daily}, \code{Often}, \code{Rare}, \code{Never}, indicating how often 
                          one only listens to music.}
    \item{\code{Daily}}{is a factor with levels \code{No}, \code{Yes} indicating if one listens to music every day.}
  }
}

\details{'NA' stands for 'not available'}

\examples{
data(Music)
str(Music)
}

\keyword{datasets}
