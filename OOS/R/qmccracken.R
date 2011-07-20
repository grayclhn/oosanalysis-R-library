nearestPercentile <- function(p, percentiles = c(75, 90, 95, 99))
  percentiles[which.min(abs(p - percentiles))]
nearestT1 <- function(val)
  pmin(pmax(2, 2 * round(val/2)), 96)
nearestT2 <- function(val)
  pmin(pmax(4, 2 * round(val/2)), 98)

critMcc1 <- function(p, k2, R, P,
                         win = c("fixed", "recursive", "rolling")) {
  win <- match.arg(win)
  p <- nearestPercentile(100 * p)
  
  R <- nearestT2(100 * R / (R+P))
  mean(mccArray[with(mccArray, t2 == R & k == k2 & window == win & quant == p),"M1b"])
}

critMcc2 <- function(p, k2, R, P1, P2,
                         win = c("fixed", "recursive", "rolling")) {
  win <- match.arg(win)
  p <- nearestPercentile(100 * p)
  
  N <- R + P1 + P2
  R1 <- nearestT1(100 * R / N)
  R2 <- nearestT2(100 * (R + P1) / N)
  mccArray[with(mccArray, t1 == R1 & t2 == R2 & k == k2 & window == win & quant == p),"M1b"]
}

