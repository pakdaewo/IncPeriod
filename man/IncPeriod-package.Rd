\name{IncPeriod-package}
\alias{IncPeriod-package}
\docType{package}
\title{Incubation Period with Uncertain Onset of Infection and Symptoms}

\description{
This package provides the distribution of incubation period with the uncertain dates of infection and symptom's onset. It allows users to utilize more data for statistical inference in infectious diseases. The association of the incubation period with patient's risk factors can be also examined using the main funciton \code{\link{IncPeriod}}. The detailed information is available in Pak et al. (2020).
}
\details{
\tabular{ll}{
Package: \tab IncPeriod\cr
Type: \tab Package\cr
Version: \tab 0.1.0\cr
Date: \tab 2022-01-03\cr
License: \tab GPL (>= 3)\cr
}
}

\author{
Daewoo Pak, Jing Ning and Yu Shen

Maintainer: Daewoo Pak <heavyrain.pak@gmail.com>
}

\references{
Pak, D., Langohr, K., Ning, J., Cortes Martinez, J., Gomez Melis, G. and Shen, Y., 2020. Modeling the coronavirus disease 2019 incubation period: impact on quarantine policy. Mathematics, 8(9), p.1631.
}

\keyword{package}

\seealso{
  \code{\link{IncPeriod}}
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

