\name{translate.logit}
\alias{translate.logit}

\title{Translates logit regression coefficients into percentages}
\description{Performs a logit regression and then computes the effects of covariates expressed in percentages
             (through two methods: 'pure' effects and 'experimental' effects; see Deauvieau, 2010)}
\usage{translate.logit(formula,data,nit=0)}

\arguments{
  \item{formula}{an object of class \code{formula} (or one that can be coerced to that class): a symbolic description of the model to be fitted.
                 Every variables have to be factors.}
  \item{data}{a data frame containing the variables in the model}
  \item{nit}{number of bootstrap iterations for confidence interval computation. Default is 0, i.e. no confidence interval is computed.}
}

\details{
This function works with binomial as well as multinomial regression models. If the dependant variable has two factors, \code{\link{glm}} is used,
if it has more than two factors \code{\link{multinom}} function (from \code{nnet} package) is used.
The function expresses the regression coefficients as percentages through three distinct methods: raw percentages, 'pure effects' percentages and 
'experimental effects' percentages (see Deauvieau, 2010). Bootstrap confidence interval are available only for binomial regressions.
}
\value{
The function returns a list:
  \item{glm}{An object of class \code{glm} or \code{nnet} (depending on the number of factors of the dependent variable)}
  \item{summary}{The results of \code{summary} function applied to \code{reg} element}
  \item{percents}{A matrix or a list of matrices (depending on the number of factors of the dependent variable)
                  with regression coefficients expressed as percentages}
  \item{boot.ci}{A matrix or a list of matrices (depending on the number of factors of the dependent variable)
                  with confidence intervals computed with bootstrap}
}
\references{
Deauvieau, J. (2010), 'Comment traduire sous forme de probabilites les resultats d'une modelisation logit ?', Bulletin of Sociological Methodology / Bulletin de Methodologie Sociologique 105(1), 5-23.

Deauvieau, J. (2011), 'Est-il possible et souhaitable traduire sous forme de probabilites un coefficient logit ? Reponse aux remarques formulees par Marion Selz a propos de mon article paru dans le BMS en 2010', Bulletin of Sociological Methodology / Bulletin de Methodologie Sociologique 112(1), 32-42.

Deauvieau, J. (2019), 'Comparer les resultats d’un modele logit dichotomique ou polytomique entre plusieurs groupes a partir
des probabilites estimees', Bulletin of Sociological Methodology / Bulletin de Methodologie Sociologique 142(1), 7-31.
}
\author{Nicolas Robette}

\seealso{
\code{\link{glm}}, \code{\link{multinom}}
}

\examples{
  ## An example for binomial logit regression
  data(Music)
  translate.logit(Daily ~ Gender + Age, Music)
  
  ## An example for multinomial logit regression
  translate.logit(OnlyMus ~ Gender + Age, Music)
}

\keyword{ multivariate }
