\name{dichotom}

\alias{dichotom}

\title{Dichotomizes the variables in a data frame}

\description{Dichotomizes the variables in a data frame exclusively composed of categorical variables}

\usage{dichotom(data, out = "numeric")}

\arguments{
  \item{data}{data frame of categorical variables}
  \item{out}{character string defining the format for dichotomized variables in the output data frame. 
             Format may be 'numeric' or 'factor' (default is 'numeric').}
}

\value{Returns a data frame with dichotomized variables. 
       The number of columns is equal to the total number of categories in the input data.}

\author{Nicolas Robette, Julien Barnier}

\examples{
## Dichotomizes 'Music' example data frame
data(Music)
dic <- dichotom(Music[,1:5])
str(dic)

## with output variables in factor format
dic <- dichotom(Music[,1:5], out='factor')
str(dic)
}

\keyword{ multivariate }
\keyword{ misc }
