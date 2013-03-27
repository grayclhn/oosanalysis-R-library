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

recursive_forecasts <- function(model, dataset, R,
                         window = c("recursive", "rolling", "fixed"),...) {
  window <- match.arg(window)
  n <- nrow(dataset)
  if (R >= n) stop("dataset must have more than R observations")
  getprediction <- function(firstobs, lastobs, horizon)
    predict(model(dataset[seq(firstobs, lastobs, by = 1),],...),
            newdata = dataset[lastobs + horizon,])
  switch(window,
         recursive = sapply((R+1):n, function(s) getprediction(1, s-1, 1)),
         rolling   = sapply((R+1):n, function(s) getprediction(s-R, s-1, 1)),
         fixed     = getprediction(1, R, 1:(n-R)))
}                 
