# IncPeriod

## I. Description
The package provides the tools for estimating the incubation period and its association with risk factors when the exact dates of infection and symptoms’ onset may not be observed, which is written in the programming language R <https://www.r-project.org>. 

## II. Installation
You can install the package by typing the following codes:

```r
library(devtools)
install_github("pakdaewo/IncPeriod")

library(IncPeriod)
?IncPeriod
```

## III. Implementation
The following information must be inputted into the main function 'IncPeriod'.

* date.exposure: a numeric matrix of patients' exposure periods. The first and second columns are start dates and end dates of the exposure, respectively.
* date.onset: a numeric vector for the exact date of symptom onset. Use NA for missing cases.
* date.hosp:  a numeric vector for the exact date of hospitalization. Use NA for missing cases.
* X: a numeric matrix for patient's covariates

Note that either date.onset or date.hosp must be provided for each patient. 


## IV. Example (covid19)

```r
library(IncPeriod)
data(covid19)

date.exposure <- covid19[, 1:2]
date.onset <- covid19[, 3]
date.hosp <- covid19[, 4]
X <- covid19[, 5:6]

res <- IncPeriod(date.exposure, date.onset, date.hosp, X)
res

summary(res)
# cplot(res, X = c(age = 42, gender = 1)) # run this code for drawing the cumulative probability of the incubation period
```
