## runs aggregate for several different functions and merges the results
## fns is a list of functions; basically we call aggregate
## for each of those functions.
aggfn <- function(frame, seriesname, bynames, fns, xnames) {
  if (!is.list(fns)) fns <- list(fns)
  x <- frame[,seriesname]
  bylist <- lapply(bynames, function(j) frame[,j])
  names(bylist) <- bynames
  ## run aggregate for each function in 'fns' and stores the result in a list
  aglist <- lapply(fns, function(f) aggregate(x, bylist, f))
  ## set outframe to be the first dataframe in the list, and rename 'x'
  outframe <- aglist[[1]]
  colnames(outframe)[length(bylist) + 1] <- xnames[1]
  k <- 1
  ## for each remaining dataframe, rename 'x' and merge with the previous frames
  while (k < length(xnames)) {
    k <- k+1
    colnames(aglist[[k]])[length(bylist) + 1] <- xnames[k]
    outframe <- merge(outframe, aglist[[k]], sort = FALSE)
  }
  outframe
}
