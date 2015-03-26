## Copyright (C) 2011-2015 Gray Calhoun; MIT license

## Returns the correlation matrix corresponding to a given covariance
## matrix V
makecorr <- function(V) {
  scalemat <- diag(1 / sqrt(diag(V)))
  crossprod(scalemat, crossprod(V, scalemat))
}
