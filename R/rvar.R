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

rvar <- function(nobs, coefficients, intercept, vcv, 
                 nburn = 1000, y0 = rep(0, nterms)) {
  nterms <- length(coefficients[[1]])
  if (any(sapply(coefficients[-1], length) != nterms))
    stop("All of the elememts of 'coefficients' must have the same length")
  neq <- length(coefficients)
  if (is.null(names(coefficients))) {
    eqnames <- paste("x", seq_len(neq), sep = "") 
  } else {
    eqnames <- names(coefficients)
  }
  nlag <- nterms %/% neq
  if (!isTRUE(all.equal(nlag * neq, nterms))) 
    stop("The number of lags must be an integer")
  y0 <- as.numeric(y0)
  y <- rbind(matrix(y0, nlag)[nlag:1,], mvrnorm(nobs + nburn, rep(0, neq), vcv))
  for (i in 1:(nobs + nburn)) {
    xvec <- as.numeric(y[i + ((nlag-1):0),])
    y[i + nlag,] <-intercept + sapply(coefficients, function(b) 
                                      crossprod(xvec, b)) + y[i + nlag,]
  }
  return(as.data.frame(do.call("cbind", lapply(1:neq, function(j) {
             jmatrix <- sapply((nlag+1):1, function(i) y[(1:nobs) + i + nburn - 1, j])
             colnames(jmatrix) <- c(eqnames[j], paste(eqnames[j], 1:nlag, sep = "L"))
             jmatrix
           }))))
}
