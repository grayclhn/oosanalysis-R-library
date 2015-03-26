## Copyright (C) 2011-2015 Gray Calhoun; MIT license

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
