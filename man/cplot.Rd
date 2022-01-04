\name{cplot}
\alias{cplot}
\alias{cplot.default}
\title{cplot method for \code{IncPeriod} object}

\description{
The \code{cplot} method for \code{\link{IncPeriod}} object for plotting the estimated culmulative function of the incubation period given the covariates.
}

\usage{
\method{cplot}{default}(object, X, conf.level = 0.95, ...)
}

\arguments{
  \item{...}{for S4 method only.}
  \item{object}{An object of class \code{IncPeriod} returned by the \code{\link{IncPeriod}} function.}
  \item{X}{A numeric vector for the covariates \code{X}.}
  \item{conf.level}{Confidence level. The default value is 0.95.}
}

\examples{
library(IncPeriod)
data(covid19)

date.exposure <- covid19[, 1:2]
date.onset <- covid19[, 3]
date.hosp <- covid19[, 4]
age <- covid19$age
gender <- covid19$gender

res <- IncPeriod(date.exposure, date.onset, date.hosp, X = cbind(age, gender))
cplot(res, c(age = 42, gender = 1))
}

\seealso{\code{IncPeriod}}
\keyword{methods}
