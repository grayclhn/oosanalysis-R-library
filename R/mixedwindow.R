## Copyright (C) 2011-2015 Gray Calhoun; MIT license

mixedwindow <- function(null, alt, dataset, R, vcv = var,
                      window = c("rolling", "fixed"), pimethod = "estimate") {
  nobs <- nrow(dataset)
  estimates <-
    mixedwindow_calculation(y = extract_target(null, dataset),
                            X = extract_predictors(null, dataset),
                            recursive_forecasts(null, dataset, R, "recursive"),
                            recursive_forecasts(alt, dataset, R, window),
                            vcv = vcv, pimethod = pimethod)
  estimates$tstat <- with(estimates, mu * sqrt((nobs - R) / avar))
  estimates$pvalue <- pnorm(estimates$tstat, lower.tail = FALSE)
  return(estimates)
}

mixedwindow_calculation <- function(y, X, forecasts.null, forecasts.alt, vcv,
                                    pimethod = "estimate") {
  X <- as.matrix(X)
  nobs <- length(y)
  noos <- length(forecasts.null)
  oos <- seq.int(to = nobs, length = noos)
  errors.null <- y[oos] - forecasts.null
  errors.alt  <- y[oos] - forecasts.alt
  forecast.differences <- forecasts.null - forecasts.alt

  pivalue <- switch(pimethod, estimate = noos / (nobs - noos), theory = Inf,
                    stop("That choice of 'pi' is unsupported."))
  dmw_calculation(f = errors.null^2 - errors.alt^2 + forecast.differences^2,
                  h = errors.null * X[oos,,drop = FALSE],
                  R = nobs - noos, vcv = vcv,
                  tBtF = 2 * solve(crossprod(X) / nobs,
                    colMeans(forecast.differences * X[oos,,drop=FALSE])),
                  pi = pivalue, window = "recursive")
}

mixedbootstrap <- function(null, alt.list, dataset, R, nboot, blocklength,
                           vcv = var, window = c("rolling", "fixed"),
                           bootstrap = c("moving", "circular", "stationary"),
                           pimethod = "estimate") {
  nobs <- nrow(dataset)
  noos <- nobs - R
  window <- match.arg(window)
  bootstrap <- match.arg(bootstrap)
  X <- extract_predictors(null, dataset)
  y <- extract_target(null, dataset)
  forecasts.null <- recursive_forecasts(null, dataset, R, "recursive")
  forecasts.alt <- lapply(alt.list, function(m) {
    recursive_forecasts(m, dataset, R, window)
  })
  makestats <- function(forecasts.null) sapply(forecasts.alt, function(alt) {
    with(mixedwindow_calculation(y, X, forecasts.null, alt, vcv),
         mu * sqrt(noos / avar), pimethod)
  })
  test.statistics <- makestats(forecasts.null)
  popmeans.boot <- makestats(predict(null(dataset),
                             newdata = dataset[seq.int(from = R+1, length = noos),]))
  
  rbootindex <- switch(bootstrap,
    moving = function() bootindex_movingblock(noos, blocklength),
    circular = function() bootindex_circularblock(noos, blocklength),
    stationary = function() bootindex_stationary(noos, blocklength))
  bootstrap.replications <- replicate(nboot, {
    bootindex <- rbootindex()
    extendedindex <- c(seq_len(R), R + bootindex)
    sapply(forecasts.alt, function(alt) {
      with(mixedwindow_calculation(y[extendedindex],
             X[extendedindex,,drop=FALSE],
             recursive_forecasts(null, dataset[extendedindex,], R, "recursive"),
             alt[bootindex], vcv, pimethod),
           mu * sqrt(noos / avar))
  })})
  return(list(statistics = test.statistics,
              replications = bootstrap.replications - popmeans.boot))
}
