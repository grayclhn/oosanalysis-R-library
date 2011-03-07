make.mccArray1 <- function(window, nsims, ngrain, kmax = 20,
                           rtRatio = seq(.01, .99, by = .01),
                           quantiles = seq(0, 1, by = 0.01),
                           showprogress = FALSE) {
  retArray <- array(dim = c(length(rtRatio), length(quantiles), kmax))
  for (k in seq_len(kmax)) {
    if (showprogress) cat(window, k, "of", kmax, "one-sample\n")
    raw <- rmcc(rtRatio, k, nsims, ngrain, window)
    ## each column of this matrix fixes the quantile and gives
    ## different values of rtRatio; each row gives different quantiles
    ## for the same rtRatio.
    retArray[,,k] <- sapply(quantiles, function(j) {
      apply((raw$gam1 - 0.5 * raw$gam2) / sqrt(raw$gam2), 1,
            function(x) quantile(x, j))})
  }
  retArray
}

make.mccArray2 <- function(window, nsims, ngrain, kmax = 20,
                           rnRatio = seq(.01, .99, by = .01),
                           quantiles = seq(0, 1, by = 0.01),
                           showprogress = FALSE) {
  if (is.unsorted(rnRatio, strictly = TRUE)) stop("'rnRatio' must be sorted.")
  retArray <- array(dim = c(length(rnRatio), length(rnRatio),
                      length(quantiles), kmax))
  for (k in seq_len(kmax)) {
    if (showprogress) cat(window, k, "of", kmax, "two-sample\n")
    raw <- rmcc(rnRatio, k, nsims, ngrain, window)
    for (i in seq_along(rnRatio)) {
      ## retArray[i,,,] is going to correspond to R/N = i%
      rn.i <- rnRatio[i]
      for (j in seq_along(rnRatio)) {
        pn.j <- rnRatio[j]
        ## retArray[,j,,] is going to correspond to P/N = i%
        qn.ij <- (1 - rn.i - pn.j)
        ## ad hoc correction for floating point imprecision.
        if (isTRUE(all.equal(0, qn.ij))) qn.ij <- 0
        retArray[i,j,,k] <- 
          if (qn.ij > 0) {
            quantile(- (sqrt(pn.j/qn.ij) * (raw$gam1[i+j,] - 0.5 * raw$gam2[i+j,]) - (raw$gam1[i,] - 0.5 * raw$gam2[i,]))
                     / sqrt(raw$gam2[i,]), quantiles)
          } else {
            NA
          }
      }
    }
  }
  retArray
}
