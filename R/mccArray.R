## Extra code to generate the mccArray objects that are used for
## quantiles for McCracken's (2007) out-of-sample test and its
## two-sample equivalent.

## 'datafile' is set at the command line.
stopifnot(is.character(datafile))

source("OOS/R/make.mccArray.R")
source("OOS/R/rmcc.R")
set.seed(810329)
rtRatio = seq(.01, .99, by = .01)
quantiles = seq(0, 1, by = 0.01)
kmax <- 12

mccArray1 <- list()
mccArray2 <- list()

mccArray1$fixed  <- mccArray1$rolling <- mccArray1$recursive <-
  array(dim = c(length(rtRatio), length(quantiles), kmax))

mccArray2$fixed <- mccArray2$rolling <- mccArray2$recursive <-
  array(dim = c(length(rtRatio), length(rtRatio), length(quantiles), kmax))

for (window in c("fixed", "recursive", "rolling")) {
  mccArray1[[window]] <- make.mccArray1(window, 2000, 10000, kmax, showprogress = TRUE)
  mccArray2[[window]] <- make.mccArray2(window, 2000, 10000, kmax, showprogress = TRUE)
}

save(mccArray1, mccArray2, file = datafile)
