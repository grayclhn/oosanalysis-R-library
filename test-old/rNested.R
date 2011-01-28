source("../R/rNested.R")

## Test for fixed window scheme
r <- rNested(n = 4000, k = 8, rtRatio = 1/2, window = "fixed")
if (t.test(r$numerator / r$denominator > - 0.029, mu = 0.10)$p.value < 0.001)
  stop("simulated value doesn't match McCracken's table")

## test for expanding window scheme
r <- rNested(n = 4000, k = 8, rtRatio = 1/2, window = "expanding")
if (t.test(r$numerator / r$denominator > - 0.131, mu = 0.10)$p.value < 0.001)
  stop("simulated value doesn't match McCracken's table")

## test for rolling window scheme
r <- rNested(n = 4000, k = 8, rtRatio = 1/2, window = "rolling")
if (t.test(r$numerator / r$denominator > - 0.408, mu = 0.10)$p.value < 0.001)
  stop("simulated value doesn't match McCracken's table")
