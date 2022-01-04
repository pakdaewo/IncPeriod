#' Title: incubation
#' @param x
#'
print.IncPeriod <- function(x, ...) {

  digit <- paste("%.", max(3, getOption("digits") - 4), "f", sep = "")

  estv <- sprintf(digit, x$est)
  sev <- sprintf(digit, x$se)
  n <- x$n
  xnames <- x$xnames
  if (is.null(xnames)) xnames <- paste("x", 1:(x$p), sep = "")

  cat("Parametric class:\n")
  cat(" the generalized odds-rate class of regression models\n")
  cat(" with three parameters (lambda, phi, rho)\n\n")
  cat("Coefficients:\n")
  xtable <- data.frame(estv, sev)
  rownames(xtable) <- c("log.lambda", "log.phi", "log.rho", xnames)
  colnames(xtable) <- c("Estimate", "Std.Error")
  print(xtable)

}
