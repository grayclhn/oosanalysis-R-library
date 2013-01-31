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

rvar <- function(nobs, coefficients, intercept, vcv, nburn = 1000,
                 y0 = matrix(0, nlag, neq)) {
  rvar.check.arguments(coefficients)
  neq <- length(coefficients)
  nlag <- length(coefficients[[1]]) / neq

  y <- rbind(matrix(as.numeric(y0), nlag)[seq.int(nlag, 1, by = -1),],
             mvrnorm(nobs + nburn, rep(0, neq), vcv))
  for (i in seq.int(1, (nobs + nburn))) {
    xvec <- as.numeric(y[seq.int(nlag + i - 1, length = nlag, by = -1),])
    y[i + nlag,] <- intercept + sapply(coefficients, function(b) 
                                       crossprod(xvec, b)) + y[i + nlag,]
  }

  rvar.lagframe(y, nobs, nlag, names(coefficients))
}

## The matrix y is just an (nobs + nburn + nlag) by neq matrix of the
## generated time series; this lagframe function makes it a data frame
## where each row contains all of the lags, for convenience in
## regressions.

rvar.lagframe <- function(y, nobs, nlag, ynames = NULL) {
  colIndex <- seq_len(ncol(y))
  zp <- expand.grid(lag = seq.int(from = 0, to = nlag, by = 1), col = colIndex)
  lag.y <- mapply(function(lag, col)
                  y[seq.int(to = nrow(y) - lag, length.out = nobs), col],
                  lag = zp$lag, col = zp$col, SIMPLIFY = FALSE)

  if (is.null(ynames)) ynames <- paste("x", colIndex, sep = "")
  names(lag.y) <- with(zp, ifelse(lag == 0, ynames[col],
                                  paste(ynames[col], lag, sep = "L")))

  as.data.frame(lag.y)
}

## Basic consistency-check of the coefficients passed to rvar
rvar.check.arguments <- function(coefficients) {
  nterms <- sapply(coefficients, length)
  neq <- length(coefficients)
  nlags <- nterms %/% neq

  ## All elememts of 'coefficients' must have the same length
  stopifnot(length(unique(nterms)) == 1)
  ## Each element of 'coefficients' must be an neq * nlag element
  ## vector
  stopifnot(isTRUE(all.equal(neq * nlags, nterms)))

  invisible(TRUE)
}
