rRollingMccracken <- function(startVals, k, nsims, ngrain) {

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
