rFixedMccracken <-  function(startVals, k, nsims, ngrain) {

  dw <- array(rnorm(k * ngrain * nsims, mean = 0, sd = 1/sqrt(ngrain)),
              c(ngrain, nsims, k))
  W <- apply(dw, c(2,3), cumsum)

  gam1 <- gam2 <- matrix(NA, length(startVals), nsims)

  for (i in 1:length(startVals)) {
    gam1[i,] <- (apply((W[ngrain,,,drop=F] - W[startVals[i],,,drop=F])
                       * W[startVals[i],,,drop=F], 2, sum)
                 * ngrain / startVals[i])
    gam2[i,] <- (apply(W[startVals[i],,,drop=F]^2, 2, sum)
                 * ngrain * (ngrain - startVals[i]) / startVals[i]^2)
  }
  list(gam1 = gam1, gam2 = gam2)
}
