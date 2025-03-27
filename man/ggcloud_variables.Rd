\name{ggcloud_variables}
\alias{ggcloud_variables}

\title{Plot of the cloud of variables}

\description{
Plots a Multiple Correspondence Analysis cloud of variables.
}

\usage{
ggcloud_variables(resmca, axes = c(1,2), points = "all", 
min.ctr = NULL, max.pval = 0.01, face = "pp",
shapes = TRUE, prop = NULL, textsize = 3, shapesize = 3,
col = NULL, col.by.group = TRUE, alpha = 1,
segment.alpha = 0.5, vlab = TRUE, sep = ".", legend = "right",
force = 1, max.overlaps = Inf)
}

\arguments{
  \item{resmca}{object created with \code{MCA}, \code{speMCA}, \code{csMCA}, \code{wcMCA}, \code{bcMCA}, \code{stMCA} or \code{multiMCA} function}
  \item{axes}{numeric vector of length 2, specifying the components (axes) to plot. Default is c(1,2).}
  \item{points}{character string. If 'all' all categories are plotted (default); if 'besth' only those who contribute most to horizontal axis are plotted; if 'bestv' only those who contribute most to vertical axis are plotted; if 'besthv' only those who contribute most to horizontal or vertical axis are plotted; if 'best' only those who contribute most to the plane are plotted.}
  \item{min.ctr}{Numerical value between 0 and 100. The minimum contribution (in percent) for a category to be displayed if the \code{points} argument is equal to "best", "besth" or "bestv" and \code{resmca} is of type \code{MCA}, \code{speMCA} or \code{csMCA}. If NULL (default), only the categories that contribute more than the average (i.e. 100 / number of modalities) are displayed.}
  \item{max.pval}{Numerical value between 0 and 100. The maximal p-value derived from test-values for a category to be displayed if the \code{points} argument is equal to "best", "besth" or "bestv" and \code{resmca} is of type \code{stMCA} or \code{multiMCA}.}
  \item{face}{character string. Changes the face of the category labels when their contribution is greater than \code{min.ctr}. The first letter refers to the first represented axis, the second letter to the second. "p" is for plain text, "u" for underlined, "i" for italic and "b" for bold. For example, "ui" means that the labels of the most contributing categories on the first axis will be underlined and the labels of the most contributing categories on the second axis will be italicized. By default ("pp"), no font face change is made.}
  \item{shapes}{Logical. Should shapes be plotted for categories (in addition to labels) ? Default is TRUE.}
  \item{prop}{If NULL, the size of the labels (if shapes=FALSE) or the shapes (if shapes=TRUE) is constant. If 'n', the size is proportional the the weights of categories; if 'ctr1', the size is proportional to the contributions of the categories on the first dimension of the plot; if 'ctr2', the size is proportional to the contributions of the categories on the second dimension of the plot; if 'ctr12', the size is proportional to the contributions of the categories on the plane ; if 'ctr.cloud', the size is proportional to the total contributions of the categories on the whole cloud; if 'cos1', the size is proportional to the quality of representation (squared cosines) of the categories on the first dimension of the plot; if 'cos2', the size is proportional to the quality of representation of the categories on the second dimension of the plot; if 'cos12', the size is proportional to the quality of representation of the categories on the plane; if 'vtest1', the size is proportional to the test-values of the categories on the first dimension of the plot; if 'vtest2', the size is proportional to the test-values of the categories on the second dimension of the plot.}
  \item{textsize}{Size of the labels of categories if shapes=TRUE, or if shapes=FALSE and prop=NULL. Default is 3.}
  \item{shapesize}{Size if the shapes of categories if shapes=TRUE and prop=FALSE. Default is 3.}
  \item{col}{Character string. Color name for the shapes and labels of the categories. If NULL (default), the default \code{ggplot2} palette is used, with one color per variable.}
  \item{col.by.group}{Logical. If \code{resmca} is of type \code{multimCA}, categories are colored by group from the MFA if TRUE (default) and by variable if FALSE.}
  \item{alpha}{Transparency of the shapes and labels of categories. Default is 1.}
  \item{segment.alpha}{Transparency of the line segment beside labels of categories. Default is 0.5.}
  \item{vlab}{Logical. Should the variable names be used as a prefix for the labels of the categories. Default is TRUE.}
  \item{sep}{Character string used as a separator if vlab=TRUE.}
  \item{legend}{the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector). Default is right.}
  \item{force}{Force of repulsion between overlapping text labels. Defaults to 1. If 0, labels are not repelled at all.}
  \item{max.overlaps}{Exclude text labels that overlap too many things. Defaults to Inf, which means no labels are excluded.}
}

\value{a \code{ggplot2} object}

\note{
If \code{col} argument is NULL, shapes or labels are colored according to the variables, using the default \code{ggplot2} palette. The palette can be customized using any \code{scale_color_*} function, such as \code{scale_color_brewer()}, \code{scale_color_grey()} or \code{scale_color_manual()}.

If \code{resmca} is of type \code{stMCA} or \code{multiMCA} and \code{points} is not equal to \code{"all"}, test-values are used instead of contributions (which are not available for these MCA variants) to select the most important categories ; if \code{points} is equal to \code{best}, only categories with high test-values for horizontal axis or vertical axis are plotted.
}

\references{
Le Roux B. and Rouanet H., \emph{Multiple Correspondence Analysis}, SAGE, Series: Quantitative Applications in the Social Sciences, Volume 163, CA:Thousand Oaks (2010).

Le Roux B. and Rouanet H., \emph{Geometric Data Analysis: From Correspondence Analysis to Stuctured Data Analysis}, Kluwer Academic Publishers, Dordrecht (June 2004).
}

\author{Anton Perdoncin, Nicolas Robette}

\seealso{
\code{\link{ggcloud_indiv}}
}

\examples{
# specific MCA of Music example data set
data(Music)
junk <- c("FrenchPop.NA", "Rap.NA", "Rock.NA", "Jazz.NA", "Classical.NA")
mca <- speMCA(Music[,1:5], excl = junk)
# cloud of variables
ggcloud_variables(mca)
# cloud of variables with only categories contributing the most
ggcloud_variables(mca, points = "best", prop = "n")
# cloud of variables with other plotting options
ggcloud_variables(mca, shapes = FALSE, legend = "none",
col = "black", face = "ui")
}

\keyword{ aplot }
\keyword{ multivariate }
