summary.IncPeriod <- function(object, figure = TRUE, plot.arg = list(), ...) {

  digit <- paste("%.", max(3, getOption("digits") - 4), "f", sep = "")
  digitp <- paste("%.", max(3, getOption("digits") - 3), "f", sep = "")

  display_p <- function(sv) {
    ifelse(as.numeric(sv) < 0.0001, "<0.0001", sv)
  }

  display_sig <- function(pvalue) {
    if (pvalue < 0.0001) {
      sigv <- "***"
    } else {
      if (pvalue < 0.001) {
        sigv <- "**"
      } else {
        if (pvalue < 0.01) {
          sigv <- "*"
        } else {
          if (pvalue < 0.1) {
            sigv <- "."
          } else sigv <- ""
        }
      }
    }

    sigv
  }

  sig <- unlist(lapply(object$pvalue, display_sig))
  estv <- sprintf(digit, object$est)
  sev <- sprintf(digit, object$se)
  zvalue <- sprintf(digit, object$est/object$se)
  pval <- display_p(sprintf(digitp, object$pvalue))
  ind_onset <- object$ind_onset
  ind_exact <- object$ind_exact
  n <- length(ind_onset)

  xnames <- object$xnames
  if (is.null(xnames)) xnames <- paste("x", 1:(object$p), sep = "")
  # summary
  cat(paste("Summary (total = ", n, "):\n", sep = ""))

  num_type1 <- sum(ind_onset & !ind_exact)
  num_type2 <- sum(!ind_onset & !ind_exact)
  num_type3 <- sum(ind_onset & ind_exact)
  num_type4 <- sum(!ind_onset & ind_exact)

  cat(paste(" - Type 1: observing potential exposure period and symptom onset date (n=", num_type1, ")\n", sep = ""))
  cat(paste(" - Type 2: observing potential exposure period and hospitalization date (n=", num_type2, ")\n", sep = ""))
  cat(paste(" - Type 3: observing exact exposure date and symptom onset date (n=", num_type3, ")\n", sep = ""))
  cat(paste(" - Type 4: observing exact exposure date and hospitalization date (n=", num_type4, ")\n\n", sep = ""))

  cat("Coefficients:\n")

  xtable <- data.frame(estv, sev, zvalue, pval, sig)
  rownames(xtable) <- c("log.lambda", "log.phi", "log.rho", xnames)
  colnames(xtable) <- c("Estimate", "Std.Error", "z.value", "Pr(>|z|)", "")
  print(xtable)

  cat("---\n")
  cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")

  if (figure == TRUE & is.list(plot.arg) == FALSE) stop("plot.arg must be a list.")
  if (figure == TRUE) {
    exposure.start <- object$exposure.start
    exposure.end <- object$exposure.end
    onset_or_hosp <- object$onset_or_hosp
    delta <- object$delta
    min_x <- min(exposure.start)
    max_x <- max(onset_or_hosp)

    if(is.null(plot.arg$xlab)) plot.arg$xlab = "Date"
    if(is.null(plot.arg$ylab)) plot.arg$ylab = ""
    if(is.null(plot.arg$main)) plot.arg$main = ""
    if(is.null(plot.arg$xlim)) plot.arg$xlim = c(min_x, max_x)
    if(is.null(plot.arg$ylim)) plot.arg$ylim = c(0, n + 30)
    if(is.null(plot.arg$axes)) plot.arg$axes = FALSE
  }

    plot.arg$x = NA
    do.call(what = plot, args = plot.arg)
    for (i in 1:n) {
      lines(c(min_x, max_x), c(i, i), lty = 3, col = "grey70")
      x_start <- exposure.start[i]
      x_end <- exposure.end[i]
      x_symptom <- onset_or_hosp[i]
      lines(x = c(x_start, x_end), c(i, i), col = "gold4", lty = 1)
      points(x_start, i, pch = 22, col = "gold4")
      lines(x = c(x_end, x_symptom), c(i, i), lty = 2, col = "darkgreen")
      if(delta[i] == 1) points(x_symptom, i, pch = 20, col = "darkgreen")
      if(delta[i] == 0) points(x_symptom, i, pch = 1, col = "darkgreen")
    }

    if(plot.arg$axes == FALSE) {
      x.axis <- seq(from = min_x, to = max_x, length.out = 6)
      # x.axis <- c(x.axis0[x.axis0 <= signif(max_x, 2)], signif(max_x, 2))
      # axis(1, x.axis, labels = x.axis)
    }

    legend("top", legend=c("exposure period","symptom onset", "hospitalization"), pch=c(22, 20, 1),
           col = c("gold4", "darkgreen", "darkgreen"), lty = c(1,  0, 0),
           inset=c(0, -0.005), xpd=TRUE, ncol = 3, cex = 1, lwd = 2)

}
