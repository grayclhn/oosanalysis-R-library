qNested2 <-
  function(x, k, rtRatio, rnRatio, win) {
    if (k != 3 & k != 13) stop("I haven't yet put in values for k equal to ", k ,"\n")
    if (!isTRUE(all.equal(x, 0.8))) stop("Only x = 0.8 is supported")
    
    nestedValues <- subset(critNested, window == win, -window)
    nestedValues$crit[apply(outer(nestedValues$rOverT, rtRatio,
                                  function(x,y) (x-y)^2) +
                            outer(nestedValues$rOverN, rnRatio,
                                  function(x,y) (x-y)^2),
                            2, which.min)]
  }
