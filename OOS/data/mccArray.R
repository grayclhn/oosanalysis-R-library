set.seed(810329)

rtRatio <- seq(.01, .99, by = .01)
quants <- seq(0, 1, by = 0.01)
kmax <- 1
k <- 1
nsims <- 2000
ngrain <- 2000

mccArray1 <- list()
mccArray2 <- list()
mccArray1$fixed  <- mccArray1$rolling <- mccArray1$recursive <-
  array(dim = c(length(rtRatio), length(quants), kmax))
mccArray2$fixed <- mccArray2$rolling <- mccArray2$recursive <-
  array(dim = c(length(rtRatio), length(rtRatio), length(quants), kmax))

for (window in c("fixed", "recursive", "rolling")) {
  rawDraws <- rmcc(rtRatio, k, nsims, ngrain, window)
  ## each column of this matrix fixes the quantile and gives different
  ## values of rtRatio; each row gives different quantiles for the
  ## same rtRatio.
  mccArray1[[window]][,,k] <- sapply(quants, function(j) {
    apply(- rawDraws$numerator / rawDraws$denominator, 1,
          function(x) quantile(x, j))})

  for (i in seq_along(rtRatio)) {
    ## mccArray2[[window]][i,,,] is going to correspond to Q/N = i%
    qn.i <- rtRatio[i]
    for (j in seq_along(rtRatio)) {
      ## mccArray2[[window]][,j,,] is going to correspond to R/N = i%
      pn.ij <- (1 - qn.i - rtRatio[j])
      mccArray2[[window]][i,j,,k] <- 
        if (pn.ij > 0 || isTRUE(all.equal(0, pn.ij))) {
          ## ad hoc correction for floating point imprecision.
          if (isTRUE(all.equal(0, pn.ij))) pn.ij <- 0
          quantile((sqrt(pn.ij/qn.i) * rawDraws$numerator[i,] - rawDraws$numerator[j,])
                   / rawDraws$denominator[j,], quants)
        } else {
          NA
        }
    }
  }
}
