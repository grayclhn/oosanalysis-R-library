## Copyright (c) 2011-2014 by Gray Calhoun
 
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

## Returns the correlation matrix corresponding to a given covariance
## matrix V
makecorr <- function(V) {
  scalemat <- diag(1 / sqrt(diag(V)))
  crossprod(scalemat, crossprod(V, scalemat))
}
