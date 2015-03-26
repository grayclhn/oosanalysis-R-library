## Copyright (C) 2011-2015 Gray Calhoun; MIT license

dmw_calculation <- function(f, h, R, vcv, tBtF = NULL, pi = noos / R,
                            window = c("recursive", "rolling", "fixed")) {
  noos <- length(f)
  htBtF <- if (is.null(tBtF)) {
    lambda <- list(fh = 0, hh = 0)
    rep(0, noos) 
  } else {
    lambda <- dmw_lambda(pi, window)
    tcrossprod(as.matrix(h), matrix(tBtF, nrow = 1))
  }
  S <- vcv(cbind(f, htBtF))
  return(list(mu = mean(f), avar = S[1,1] + lambda$fh * (S[1,2] + S[2,1]) 
                                   + lambda$hh * S[2,2]))
}

dmw_lambda <- function(pi, window = c("recursive", "rolling", "fixed")) {
  window <- match.arg(window)
  stopifnot(pi >= 0)
  if (window == "recursive") {
    if (pi >= Inf) {
      lambda.fh <- 1
    } else if (pi > 0) {
      lambda.fh <- 1 - log(1 + pi)/pi
    } else {
      lambda.fh <- 0
    }
    lambda.hh <- 2 * lambda.fh
  } else if (window == "fixed") {
    lambda.fh <- 0
    lambda.hh <- pi
  } else if (window == "rolling" && pi <= 1) {
    lambda.fh <- pi/2
    lambda.hh <- pi - pi^2 / 3
  } else if (window == "rolling" && pi > 1) {
    lambda.fh <- 1 - 1/(2*pi)
    lambda.hh <- 1 - 1/(3*pi)
  }
  return(list(fh = lambda.fh, hh = lambda.hh))
}

dmw_mse <- function(null, alt, dataset, R, vcv = var,
                    window = c("recursive", "rolling", "fixed")) {

  y <- extract_target(null, dataset)
  X <- extract_predictors(null, dataset)
  forecasts.null <- recursive_forecasts(null, dataset, R, window)
  forecasts.alt <- recursive_forecasts(alt, dataset, R, window)
  
  nobs <- length(y)
  noos <- length(forecasts.null)
  oos <- seq.int(to = nobs, length = noos)

  errors.null <- y[oos] - forecasts.null
  errors.alt  <- y[oos] - forecasts.alt

  estimates <- dmw_calculation(f = errors.null^2 - errors.alt^2,
                               h = errors.null * X[oos,,drop = FALSE],
                               R = nobs - noos, vcv = vcv, 
                               pi = noos / (nobs - noos), window = "recursive")
  estimates$tstat <- with(estimates, mu * sqrt((nobs - R) / avar))
  estimates$pvalue <- pnorm(estimates$tstat, lower.tail = FALSE)
  return(estimates)
}
