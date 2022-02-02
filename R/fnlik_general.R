fnlik_general <- function(par, dat, nosum = FALSE) {

  lambda <- exp(par[1])
  phi <- exp(par[2])
  rho <- exp(par[3])
  beta <- par[-c(1:3)]

  n <- nrow(dat)
  ra <- dat[, 1]
  da <- dat[, 2]
  ie <- dat[, 3] # d == 0, so the values are not interval-censored
  io <- dat[, 4] # onset == 1 and hospitalization == 0
  if (length(beta) !=0) {
    z <- matrix(dat[,4 + (1:length(beta))], ncol = length(beta))
    expzb <- as.numeric(exp(z %*% beta))
  } else {
    expzb <- rep(1, n)
  }

  ft <- function(t, lambda, phi, rho, expzb) {
    phi * lambda^(-phi) * t^(phi - 1) * expzb * ( 1 + rho * (t/lambda)^phi * expzb)^(-(1+rho)/rho)
  }

  St <- function(t, lambda, phi, rho, expzb) {
    (1 + rho * (t/lambda)^phi * expzb)^(-1/rho)
  }

  # Category 1
  ind_c1 <- (ie == 0) & (io == 1)
  if(any(ind_c1)) {
    r_c1 <- ra[ind_c1]
    d_c1 <- da[ind_c1]
    expzb_c1 <- expzb[ind_c1]
    logL1 <- log(St(r_c1, lambda, phi, rho, expzb_c1) - St(r_c1 + d_c1, lambda, phi, rho, expzb_c1))
  } else {
    logL1 <- 0
  }

  # Category 2
  ind_c2 <- (ie == 0) & (io == 0)
  if(any(ind_c2)) {
    r_c2 <- ra[ind_c2]
    d_c2 <- da[ind_c2]
    expzb_c2 <- expzb[ind_c2]
    L2 <- c()
    for (i in 1:length(r_c2)) {
      L2[i] <- 1 - 1/d_c2[i] * integrate(St, 0, d_c2[i], lambda = lambda, phi = phi, rho = rho, expzb = expzb_c2[i])$value
    }
    logL2 <- log(L2)
  } else {
    logL2 <- 0
  }

  # Category 3
  ind_c3 <- (ie == 1) & (io == 1)
  if(any(ind_c3)) {
    r_c3 <- ra[ind_c3]
    d_c3 <- da[ind_c3]
    expzb_c3 <- expzb[ind_c3]
    logL3 <- log(ft(r_c3, lambda, phi, rho, expzb_c3))
  } else {
    logL3 <- 0
  }

  # Category 4
  ind_c4 <- (ie == 1) & (io == 0)
  if(any(ind_c4)) {
    r_c4 <- ra[ind_c4]
    d_c4 <- da[ind_c4]
    expzb_c4 <- expzb[ind_c4]
    logL4 <- log(1 - St(r_c4, lambda, phi, rho, expzb_c4))
  } else {
    logL4 <- 0
  }

  # total
  logL <- rep(0, n)
  logL[ind_c1] <- logL1
  logL[ind_c2] <- logL2
  logL[ind_c3] <- logL3
  logL[ind_c4] <- logL4

  if(nosum == FALSE) {
    return(-sum(logL))
  } else {
    return(-logL)
  }

}
