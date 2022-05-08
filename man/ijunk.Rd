\name{ijunk}

\alias{ijunk}

\title{Defining junk categories for specific MCA}

\description{This function launches a shiny app to define interactively the junk categories before a specific MCA.}

\usage{ijunk(data, init_junk = NULL)}

\arguments{
  \item{data}{data frame of categorical variables to be used in a specific MCA}
  \item{init_junk}{optional vector of junk categories. Can be a numeric vector indicating the indexes of the junk categories or a character vector of junk categories, specified in the form "namevariable.namecategory" (for instance "gender.male"). Default is NULL.}
}

\details{Once the selection of junk categories is interactively done, the function provides the code to use in a script. It also offer the opportunity to select a set of junk categories at once by writing the common suffix of these categories.}

\value{A character vector of junk categories.}

\author{Nicolas Robette}

\seealso{
\code{\link{speMCA}}, \code{\link{csMCA}}
}

\examples{
# NOT RUN {
data(Music)
ijunk(Music[,1:5])
# or
junk <- ijunk(Music[,1:5])
# To update an existing vector of junk categories
junk <- ijunk(Music[,1:5], init_junk = c("Rock.NA", "Rap.NA"))
# and then
mca <- speMCA(Music[,1:5], excl = junk)
# }
}

\keyword{ multivariate }
\keyword{ misc }
