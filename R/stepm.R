## Copyright (c) 2011-2013 by Gray Calhoun
 
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or (at
## your option) any later version.

## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
## General Public License for more details.

## For a copy of the GNU General Public License, please see
## <http://www.r-project.org/Licenses/>.

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
