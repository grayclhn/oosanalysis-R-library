make.mccArray1 <- function(window, nsims, ngrain, kmax = 20,
                           rtRatio = seq(.01, .99, by = .01),
                           quantiles = seq(0, 1, by = 0.01)) {
  retArray <- array(dim = c(length(rtRatio), length(quantiles), kmax))
  for (k in seq_len(kmax)) {
    rawDraws <- rmcc(rtRatio, k, nsims, ngrain, window)
    ## each column of this matrix fixes the quantile and gives
    ## different values of rtRatio; each row gives different quantiles
    ## for the same rtRatio.
    retArray[,,k] <- sapply(quantiles, function(j) {
      apply(- rawDraws$numerator / rawDraws$denominator, 1,
            function(x) quantile(x, j))})
  }
  retArray
}

make.mccArray2 <- function(window, nsims, ngrain, kmax = 20,
                           rtRatio = seq(.01, .99, by = .01),
                           quantiles = seq(0, 1, by = 0.01)) {

  retArray <- array(dim = c(length(rtRatio), length(rtRatio),
                      length(quantiles), kmax))
  for (k in seq_len(kmax)) {
    rawDraws <- rmcc(rtRatio, k, nsims, ngrain, window)
    for (i in seq_along(rtRatio)) {
      ## retArray[i,,,] is going to correspond to Q/N = i%
      qn.i <- rtRatio[i]
      for (j in seq_along(rtRatio)) {
        ## retArray[,j,,] is going to correspond to R/N = i%
        pn.ij <- (1 - qn.i - rtRatio[j])
        retArray[i,j,,k] <- 
          if (pn.ij > 0 || isTRUE(all.equal(0, pn.ij))) {
            ## ad hoc correction for floating point imprecision.
            if (isTRUE(all.equal(0, pn.ij))) pn.ij <- 0
            quantile((sqrt(pn.ij/qn.i) * rawDraws$numerator[i,] - rawDraws$numerator[j,])
                     / rawDraws$denominator[j,], quantiles)
          } else {
            NA
          }
      }
    }
  }
  retArray
}
