rmcc <-
function(n, k, rtRatio, window, ngrain = 400)
{
  ## startVals is the rtRatio, but rescaled so that it applies for
  ## ngrain -- we also ensure that startVals is positive.
  startVals <- sapply(floor(rtRatio * ngrain), function(x) max(x, 1))
  rvPair <- switch(window,
                   "fixed"     = rmcc.fixed(    startVals, k, n, ngrain),
                   "rolling"   = rmcc.rolling(  startVals, k, n, ngrain),
                   "expanding" = rmcc.expanding(startVals, k, n, ngrain),
                   stop("Invalid window type"))
  list(numerator = rvPair$gam1 - 0.5 * rvPair$gam2,
       denominator = sqrt(rvPair$gam2))
}

rmcc.expanding <- function(startVals, k, nsims, ngrain) {

  dw <- array(rnorm(k * ngrain * nsims, mean = 0, sd = 1/sqrt(ngrain)),
              c(ngrain, nsims, k))

  wOverS <- (ngrain * apply(dw, c(2,3), cumsum) *
             array(1 / (1:ngrain), c(ngrain, nsims, k)))

  G1steps <- wOverS[-ngrain,,,drop = FALSE] * dw[-1,,,drop = FALSE]
  G2steps <- wOverS[-ngrain,,,drop = FALSE]^2 / ngrain

  gam1 <- gam2 <- matrix(NA, length(startVals), nsims)
  for (i in 1:length(startVals)) {
    gam1[i,] <- apply(G1steps[startVals[i]:ngrain - 1,,,drop=FALSE], 2, sum)
    gam2[i,] <- apply(G2steps[startVals[i]:ngrain - 1,,,drop=FALSE], 2, sum)
  }
  list(gam1 = gam1, gam2 = gam2)
}

rmcc.fixed <-  function(startVals, k, nsims, ngrain) {

  dw <- array(rnorm(k * ngrain * nsims, mean = 0, sd = 1/sqrt(ngrain)),
              c(ngrain, nsims, k))
  W <- apply(dw, c(2,3), cumsum)

  gam1 <- gam2 <- matrix(NA, length(startVals), nsims)

  for (i in 1:length(startVals)) {
    gam1[i,] <- (apply((W[ngrain,,,drop=FALSE] - W[startVals[i],,,drop=FALSE])
                       * W[startVals[i],,,drop=FALSE], 2, sum)
                 * ngrain / startVals[i])
    gam2[i,] <- (apply(W[startVals[i],,,drop=FALSE]^2, 2, sum)
                 * ngrain * (ngrain - startVals[i]) / startVals[i]^2)
  }
  list(gam1 = gam1, gam2 = gam2)
}
  
rmcc.rolling <- function(startVals, k, nsims, ngrain) {

  ## generate the brownian motion increments
  dw <- array(rnorm(k * ngrain * nsims, mean = 0, sd = 1/sqrt(ngrain)),
              c(ngrain, nsims, k))

  ## sum the increments to get the weighted browian motion
  w <- apply(dw, c(2,3), cumsum)

  gam1 <- gam2 <- matrix(NA, length(startVals), nsims)
  for (i in 1:length(startVals)) {
    gam1[i,] <- (apply((w[startVals[i]:ngrain - 1,,,drop = FALSE] -
                        w[1:(ngrain-startVals[i] + 1),,,drop = FALSE])
                       * dw[startVals[i]:ngrain,,,drop = FALSE], 2, sum)
                 * ngrain / startVals[i])
    gam2[i,] <- (apply((w[startVals[i]:ngrain - 1,,,drop = FALSE] -
                        w[1:(ngrain-startVals[i] + 1),,,drop = FALSE])^2, 2, sum)
                 * ngrain / startVals[i]^2)
  }
  list(gam1 = gam1, gam2 = gam2)
}
