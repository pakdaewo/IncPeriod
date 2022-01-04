fun_F <- function(est, t, inputX) {
  lambda <- exp(est[1])
  phi <- exp(est[2])
  rho <- exp(est[3])
  b <- est[-c(1:3)]
  bX <- sum(b * inputX)
  1 - (1 + rho * (t/lambda)^phi * exp(bX))^(-1/rho)

}
