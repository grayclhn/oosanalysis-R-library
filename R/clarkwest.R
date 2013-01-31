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

clarkwest <- function(null, alt, dataset, R, vcv = var,
                      window = c("rolling", "recursive", "fixed"))
  clarkwest_calculation(
    target        = extract_target(null, dataset[-seq_len(R),,drop = FALSE]),
    null.forecast = recursive_forecasts(null, dataset, R, window),
    alt.forecast  = recursive_forecasts(alt, dataset, R, window),
    vcv)

clarkwest_calculation <- function(target, null.forecast, 
                                  alt.forecast, vcv) {
  P <- length(target)
  oos.sequence <- {(target - null.forecast)^2 - 
                   (target - alt.forecast)^2 + 
                   (null.forecast - alt.forecast)^2}
  mu <- mean(oos.sequence)
  avar <- vcv(oos.sequence)
  return(list(mu = mu, avar = avar, 
              pvalue = pnorm(sqrt(P) * mu, 0, sqrt(avar), FALSE)))
}
