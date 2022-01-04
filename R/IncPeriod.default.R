IncPeriod.default <- function(date.exposure, date.onset, date.hosp, X, ...) {

  match.call()

  if (missing(date.exposure)|any(is.na(date.exposure))) stop("exposure must be provided (no missing value).")
  exposure.start <- as.Date(date.exposure[, 1])
  exposure.end <- as.Date(date.exposure[, 2])
  onset <- as.Date(date.onset)
  hosp <- as.Date(date.hosp)
  n <- length(exposure.start)

  onset_or_hosp <- as.Date(pmin(onset, hosp, na.rm = TRUE))
  delta <- rep(NA, n)
  delta[onset_or_hosp == hosp] <- 0
  delta[onset_or_hosp == onset] <- 1

  if (any(is.na(delta))|(length(delta) != n)) stop("either onset date or hospitalization date must be provided")

  d <- as.numeric(exposure.end - exposure.start)
  r <- as.numeric(onset_or_hosp - exposure.end)
  ind_onset <- as.numeric(delta)
  ind_exact <- as.numeric(d == 0)
  x <- as.matrix(X)
  p <- ncol(x)
  dat <- as.matrix(cbind(r = r, d=d, ie = ind_exact, io = ind_onset, x))

  fit <- optim(par = c(0.01, 0.01, -0.01, rep(0, p)), fn = fnlik_general, dat = dat, control = list(maxit = 10000))
  est <- fit$par
  hess <- hessian(func = fnlik_general, x = est, dat = dat)
  se <-  sqrt(diag(solve(hess)))
  pvalue <- 2*(1 - pnorm(abs(est/se)))

  results <- list()
  results$fit <- fit
  results$est <- est
  results$hess <- hess
  results$se <- se
  results$pvalue <- pvalue
  results$ind_onset <- ind_onset
  results$ind_exact <- ind_exact
  results$d <- d
  results$r <- r
  results$n <- n
  results$p <- p
  results$exposure.start <- exposure.start
  results$exposure.end <- exposure.end
  results$onset_or_hosp <- onset_or_hosp
  results$delta <- delta
  results$xnames <- colnames(X)

  class(results) <- "IncPeriod"

  results

}
