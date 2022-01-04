cplot.default <- function(object, X, conf.level = 0.95, ...) {

  match.call()

  if(!(class(object) %in% c("IncPeriod"))) stop("Please input the object returned by 'IncPeriod'.")

  dotlist <- list(...)
  if (any(dotlist == "type")) stop("The graphical argument 'type' is not allowed.")

  if (missing(X)) {
    inputX = 0
  } else {
    if (is.null(object$xnames)) {
      inputX <- X
    } else {
      xnames <- object$xnames
      inputX <- X[xnames]
    }
  }

  dotnames <- names(list(...))

  max_x <- max(object$onset_or_hosp - object$exposure.start, na.rm = TRUE)

  est <- object$est
  hess <- object$hess
  alpha <- 1 - conf.level
  zv <- qnorm(1 - alpha/2)
  t.set <- seq(from = 0, to = max_x, length.out = 100)
  res <- matrix(NA, nrow = length(t.set), ncol = 3)

  for (tt in 1:length(t.set)){
    t.point <- t.set[tt]
    Fv <- fun_F(est, t = t.point, inputX)
    Fv_grad <- grad(func = fun_F, x = est, t = t.point, inputX = inputX)
    Fv_se <- sqrt(as.numeric(matrix(Fv_grad, 1) %*% solve(hess) %*% matrix(Fv_grad, ncol = 1)))
    Fv_ci <- Fv + c(-1, 1) * zv * Fv_se
    res[tt,] <- c(Fv, Fv_ci)
  }

  res[, 2] <- pmax(res[, 2], 0)
  res[, 3] <- pmin(res[, 3], 1)
  ub_xlim <- ceiling(min(t.set[max(which(res[, 1] < 0.99))], max_x))
  if(is.null(dotlist$xlab)) dotlist$xlab = "Time"
  if(is.null(dotlist$ylab)) dotlist$ylab = "Cumulative probability"
  if(is.null(dotlist$main)) dotlist$main = ""
  if(is.null(dotlist$xlim)) dotlist$xlim = c(0, ub_xlim)
  if(is.null(dotlist$axes)) dotlist$axes = FALSE
  if(is.null(dotlist$ylim)) dotlist$ylim = c(0, max(res[,3]))
  dotlist$x = NA

  do.call(what = plot, args = dotlist)
  # polygon(c(t.set, rev(t.set)), c(res[, 2], rev(res[, 3])), col = rgb(0.7, 0.7, 0.7,0.5), border = FALSE)
  lines(t.set, res[, 1], type = 'l')
  lines(t.set, res[, 2], type = 'l', lty = 2)
  lines(t.set, res[, 3], type = 'l', lty = 2)

  x.max <- dotlist$xlim[2]
  y.max <- dotlist$ylim[2]

  if(dotlist$axes == FALSE) {
    x.axis0 <- signif(seq(from = 0, to = x.max, length.out = 6),2)
    x.axis <- c(x.axis0[x.axis0 <= signif(x.max, 2)], signif(x.max, 2))
    y.axis0 <- signif(seq(from = 0, to = y.max, length.out = 6),2)
    y.axis <- c(y.axis0[y.axis0 <= signif(y.max, 2)], signif(y.max, 2))
    axis(1, x.axis)
    axis(2, y.axis)
  }

}

