rExpandingMccracken <- function(startVals, k, nsims, ngrain) {

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
