\name{covid19}
\alias{covid19}
\docType{data}
\title{The data set for the confirmed COVID-19 cases with potential exposure periods}
\description{
The clinically confirmed cases with symptoms were collected from two sources (DXY; Xu et al., 2020), available as of March 2020.
}
\usage{data(covid19)}
\format{
  The data have the following columns:
  \describe{
    \item{\code{exposure.start}}{exposure start date}
    \item{\code{expoosure.end}}{exposure end date}
    \item{\code{onset}}{symptom onset date}
    \item{\code{hospital}}{hospitalization date}
    \item{\code{age}}{patient's age}
    \item{\code{gender}}{patient's gender (1 for male and 0 for female)}
  }
}
\examples{
library(IncPeriod)
data(covid19) # 312 subjects and 2 covariates (age and gender)
head(covid19)

}

\keyword{datasets}


\references{
DXY: COVID-19 Epidemic Situation in Real Time. Available online: https://docs.google.com/spreadsheets
/d/e/2PACX-1vQU0SIALScXx8VXDX7yKNKWWPKE1YjFlWc6VTEVSN45CklWWf-uWmprQIyLtoPDA
18tX9cFDr-aQ9S6/pubhtml (accessed on 20 March 2020).

Xu, B., Gutierrez, B., Mekaru, S., Sewalk, K., Goodwin, L., Loskill, A., Cohn, E.L., Hswen, Y., Hill, S.C., Cobo, M.M. and Zarebski, A.E., 2020. Epidemiological data from the COVID-19 outbreak, real-time case information. Scientific data, 7(1), pp.1-6.

}
