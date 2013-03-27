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

mccracken_criticalvalue <- 
  function(pi, k2, confidence, window = c("recursive", "rolling", "fixed")) {
    window <- match.arg(window)
    confidenceIndex <- match(confidence, c(0.99, 0.95, 0.90))
    if (pi <= PiBounds[1]) {
      return(McCrackenData[[window]][1, confidenceIndex, k2])
    } else if (pi >= PiBounds[2]) {
      return(McCrackenData[[window]][length(PiIntervals), confidenceIndex, k2])
    } else {
      leftIndex <- findInterval(pi, PiIntervals)
      piLeft <- PiIntervals[leftIndex]
      wRight <- (pi - piLeft) / (PiIntervals[leftIndex + 1] - piLeft)
      critLeft <- McCrackenData[[window]][leftIndex, confidenceIndex, k2]
      critRight <- McCrackenData[[window]][leftIndex + 1, confidenceIndex, k2]
      return((1 - wRight) * critLeft + wRight * critRight)
    }
  }
