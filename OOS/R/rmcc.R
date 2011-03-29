rmcc <- function(rtRatio, k, nsims = 600, ngrain = 600,
                 window = c("fixed", "recursive", "rolling")) {
  window <- match.arg(window)
  ## startVals is the rtRatio, but rescaled so that it applies for
  ## ngrain -- we also ensure that startVals is positive.  
  startVals <- unique(pmax(1, floor(rtRatio * ngrain)))
  switch(window,
         "fixed"     = rmcc.fixed(startVals, k, nsims, ngrain),
         "rolling"   = rmcc.rolling(startVals, k, nsims, ngrain),
         "recursive" = rmcc.recursive(startVals, k, nsims, ngrain))
}

rmcc.recursive <- function(startVals, k, nsims, ngrain) {

  dw <- array(rnorm(k * ngrain * nsims, mean = 0, sd = 1/sqrt(ngrain)),
              c(ngrain, nsims, k))

  ## don't be alarmed by the 1/(1:ngrain); we're not going to use all
  ## of these terms.
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
    iLead <- (startVals[i]+1):(ngrain-1)
    iLag <- 1:(ngrain-startVals[i]-1)
    gam1[i,] <-
      (apply((w[iLead,,,drop = FALSE] - w[iLag,,,drop = FALSE]) *
             dw[iLead+1,,,drop = FALSE], 2, sum) * ngrain / startVals[i])
    gam2[i,] <-
      (apply((w[iLead,,,drop = FALSE] - w[iLag,,,drop = FALSE])^2, 2, sum)
       * ngrain / startVals[i]^2)
  }
  list(gam1 = gam1, gam2 = gam2)
}
