\name{Movies}

\alias{Movies}

\docType{data}

\title{Movies (data)}

\description{
The data concerns a sample of 1000 Movies which were on screens in France and come of their characteristics.
}

\usage{data(Movies)}

\format{
  A data frame with 1000 observations and the following 7 variables:
  \describe{
    \item{\code{Budget}}{numeric vector of movie budgets}
    \item{\code{Genre}}{is a factor with 9 levels}
    \item{\code{Country}}{is a factor with 4 level. Country of origin of the movie.}
    \item{\code{ArtHouse}}{is a factor with levels \code{No}, \code{Yes}. Whether the movie had the "Art House" label.}
    \item{\code{Festival}}{is a factor with levels \code{No}, \code{Yes}. Whether the movie was selected in Cannes, Berlin or Venise film festivals.}
    \item{\code{Critics}}{numeric vector of average ratings from intellectual criticism.}
    \item{\code{BoxOffice}}{numeric vector of number of admissions.}
}
}

\examples{
data(Movies)
str(Movies)
}

\keyword{datasets}
