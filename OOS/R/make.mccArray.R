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
      r.i <- rnRatio[i]
      for (j in seq_along(rnRatio)) {
        p1.j <- rnRatio[j]
        ## retArray[,j,,] is going to correspond to P/N = i%
        p2.ij <- (1 - r.i - p1.j)
        ## ad hoc correction for floating point imprecision.
        if (isTRUE(all.equal(0, p2.ij))) p2.ij <- 0
        retArray[i,j,,k] <- 
          if (p2.ij > 0) {
            ## need to rescale G1, G2, L1, and L2 to agree with the
            ## paper's notation.
            rescale <- 1 / (p1.j + r.i)
            G1 <- rescale * raw$gam1[i,]
            G2 <- rescale * raw$gam2[i,]
            L1 <- rescale * raw$gam1[i+j,]
            L2 <- rescale * raw$gam2[i+j,]
            quantile((G1 - G2/2 - sqrt(p1.j/p2.ij) * (L1 - L2/2)) / sqrt(G2),
                     quantiles)
          } else {
            NA
          }
      }
    }
  }
  retArray
}
