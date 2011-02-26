genNested <- function(k, seed = 88, ngrain = 800, nsims = 2000) {
  set.seed(seed)
  equalEnough <- function(x, y)
    apply(cbind(x,y), 1, function(a) isTRUE(all.equal(a[1], a[2], check.attributes = FALSE)))

  rn <- seq(0.02, 0.98, by = 0.02)
  windows <- c("expanding", "rolling", "fixed")
  critNested <- expand.grid(rOverT = rn, rOverN = c(0,rn), window = windows)
  critNested <- critNested[critNested$rOverN < critNested$rOverT | critNested$window != "rolling",]
  critNested$k <- k
  critNested$crit <- NA
  attach(critNested)

  pOverTau <- ifelse(window == "rolling", (rOverT/rOverN - 1)^(-1) - (1/rOverN - 1/rOverT)^(-1), (rOverT - 1) / (1 - 1/rOverN))
  

  for (win in windows) {
    rv1 <- rNested(nsims, k, rn, win, ngrain)
    rv2 <- rNested(nsims, k, rn, win, ngrain)

    ## these are the critical values for the single sample statistic statistic.
    critNested$crit[equalEnough(0, rOverN) & window == win] <-
      apply(- rv1$numerator / rv1$denominator, 1, function(x) quantile(x, 0.2))

    index <- window == win & !equalEnough(0, rOverN)
    rvNum1  <- rv1$numerator[  match(rOverT[index], rn),, drop = FALSE]
    rvDenom <- rv1$denominator[match(rOverT[index], rn),, drop = FALSE]
    rvNum2  <- rv2$numerator[  match(rOverN[index], rn),, drop = FALSE]

    critNested$crit[index] <-
      apply((pOverTau[index] * rvNum2 - rvNum1) / rvDenom, 1, function(x) quantile(x, 0.2))
  }
  detach("critNested")
  critNested
}
