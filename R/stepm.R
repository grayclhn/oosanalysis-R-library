## Copyright (C) 2011-2015 Gray Calhoun; MIT license

stepm <- function(teststatistics, bootmatrix, lefttail, righttail) {
  nstatistics <- length(teststatistics)
  rejected <- rep(FALSE, nstatistics)
  nrejected.endofloop <- 0
  repeat {
    nrejected.topofloop <- nrejected.endofloop
    bootmax <- apply(bootmatrix[!rejected,, drop = FALSE], 2, max)
    bootmin <- apply(bootmatrix[!rejected,, drop = FALSE], 2, min)
    rightcrit <- if (is.na(righttail)) Inf else 
                   quantile(bootmax, 1 - righttail)
    leftcrit <- if (is.na(lefttail)) -Inf else 
                  quantile(bootmin[bootmax <= rightcrit], 
                           lefttail / (1 - righttail))
    rejected[teststatistics < leftcrit | teststatistics > rightcrit] <- TRUE
    nrejected.endofloop <- sum(rejected)
    if (nrejected.endofloop == nrejected.topofloop ||
        nrejected.endofloop >= nstatistics) break
  }
  return(list(leftcrit = leftcrit, rightcrit = rightcrit, 
              rejected = rejected))
}
