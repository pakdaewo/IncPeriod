\name{summary.IncPeriod}
\alias{summary.IncPeriod}
\title{Summary method for \code{IncPeriod} object}

\description{
The \code{summary} method for \code{\link{IncPeriod}} object for summarizing input data.
}

\usage{
\method{summary}{IncPeriod}(object, figure = TRUE, plot.arg = list(), ...)
}

\arguments{
  \item{...}{for S4 method only.}
  \item{object}{An object of class \code{\link{IncPeriod}} returned by the \code{IncPeriod} function.}
  \item{figure}{A logical value drawing the figure for exposure to symptom onset timeline. Default is \code{TRUE}.}
  \item{plot.arg}{A list for the arguments for the \code{plot} function.}
}

\examples{
library(IncPeriod)
data(covid19)

date.exposure <- covid19[, 1:2]
date.onset <- covid19[, 3]
date.hosp <- covid19[, 4]
X <- covid19[, 5:6]

res <- IncPeriod(date.exposure, date.onset, date.hosp, X)
summary(res)
}

\seealso{\code{IncPeriod}}
