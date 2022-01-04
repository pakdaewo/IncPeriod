\name{IncPeriod}
\alias{IncPeriod}
\alias{IncPeriod.default}
\alias{print.IncPeriod}
\title{The main function of the package provides the estimation of the incubation period of an infectious disease}
\description{
The function provides the distribution of incubation and its association with risk factors by using the best available information from patients. The patients, included in the analysis, must be one of the following four types:
\itemize{
  \item Type 1: observing potential exposure period and symptom onset date
  \item Type 2: observing potential exposure period and hospitalization date
  \item Type 3: observing exact exposure date and symptom onset date
  \item Type 4: observing exact exposure date and hospitalization date
}
}
\usage{
\method{IncPeriod}{default}(date.exposure, date.onset, date.hosp, X, ...)
}
\arguments{
  \item{...}{for S4 method only.}
  \item{date.exposure}{A numeric matrix for patients' exposure periods. The first column of the matrix is start dates of the exposure period and the second column is the end dates of the exposure period with no missing values.}
  \item{date.onset}{A numeric vector for the exact date of symptom onset. Need to impute \code{NA} for missing cases}
  \item{date.hosp}{A numeric vector for the exact date of hospitalization. Need to impute \code{NA} for missing cases. It must be provided if a patient's exact symptom onset is not supplied with the argument \code{date.onset}.}
  \item{X}{A numeric matrix for the covariates.}
}

\details{
Within the generalized odds-rate class of regression models, the conditional probability density function \eqn{t} given \eqn{x} is:

\eqn{f(t|x) = \phi \lambda^{-\phi} t^{\phi - 1} exp(x'\beta)\{1 + \rho(t/\lambda)^\phi exp(x'\beta)\}^{-(1+\rho)/\rho}.
}


Use the \code{summary} function to see the summaries of the input data and results. Use the \code{\link{cplot}} function to plot the estimate of the cumlative probability given the covariates. See the usages in the examples given below.
}

\seealso{
  \code{\link{cplot}}
}

\examples{
library(IncPeriod)
data(covid19)

date.exposure <- covid19[, 1:2]
date.onset <- covid19[, 3]
date.hosp <- covid19[, 4]
X <- covid19[, 5:6]

res <- IncPeriod(date.exposure, date.onset, date.hosp, X)
res

summary(res)
cplot(res, X = c(age = 42, gender = 1))
}

