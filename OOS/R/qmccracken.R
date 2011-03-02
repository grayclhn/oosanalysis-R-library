interpolate <- function(at, coords, values) {
  if (length(values) != nrow(coords))
    stop("'values' must have as many elements as 'coords' has rows.")
  if (length(at) != ncol(coords))
    stop("'at' must have as many elements as 'coords' has columns.")
  for (j in seq_along(coords)) {
    index.j <- coords[,1] == coords[1,1]
    coord.j <- unique(coords[,1])
    coords <- unique(coords[,-1,drop = FALSE])
    if (length(coord.j) != 2)
        stop("each column of 'coords' must have exactly two unique entries.")
    values <- cbind(values[index.j], values[!index.j]) %*%
        abs(c(coord.j[2] - at[j], coord.j[1] - at[j]) / diff(coord.j))
  }
  drop(values)
}

index.p <- function(p) {  
  pI <- 100 * p + 1
  pvec <- c(ceiling(pI - 1), ceiling(pI))
  if (any(pvec < 1)) {
    pvec <- c(1,2)
  } else if (any(pvec > 101)) {
    pvec <- c(100, 101)
  }
  c(pI, pvec)
}

index.rt <- function(rt) {  
  rtI <- 100 * rt
  rtVec <- c(ceiling(rtI - 1), ceiling(rtI))
  if (any(rtVec < 1)) {
    rtVec <- c(1,2)
  } else if (any(rtVec > 99)) {
    rtVec <- c(98, 99)
  }
  c(rtI, rtVec)
}

qmccracken.1 <- function(p, k, rtRatio, 
                         window = c("fixed", "recursive", "rolling")) {
  window <- match.arg(window)
  data(mccArray1)
  ## we've tabluated values for p = 0, 0.01, ..., 1.00 and for
  ## rtRatio = 0.01,...,0.99
  p <- index.p(p)
  rt <- index.rt(rtRatio)

  coords <- expand.grid(p = p[-1], rt = rt[-1])
  values <- mapply(function(p, rt) mccArray1[[window]][rt, p, k],
                   p = coords$p, rt = coords$rt)
  interpolate(c(p[1]+1, rt[1]), coords, values)
}

qmccracken.2 <- function(p, k, rtRatio, qtRatio,
                         window = c("fixed", "recursive", "rolling")) {
  if (qtRatio < 0 || rtRatio < 0 || rtRatio > 1)
    stop("rtRatio and qtRatio must be positive and rtRatio must be less than one.")
  window <- match.arg(window)
  qnRatio <- qtRatio / (1 + qtRatio)
  rnRatio <- (1 - qnRatio) * rtRatio
  ## we've tabluated values for p = 0, 0.01, ..., 1.00 and for
  ## qnRatio and rnRatio = 0.01,...,0.99
  p <- index.p(p)
  rn <- index.rt(rnRatio)
  qn <- index.rt(qnRatio)

  coords <- expand.grid(p = p[-1], rn = rn[-1], qn = qn[-1])
  values <- mapply(function(p, rn, qn) mccArray2[[window]][qn, rn, p, k],
                   p = coords$p, rn = coords$rn, qn = coords$qn)
  interpolate(c(p[1], rn[1], qn[1]), coords, values)
}

searchAlg <- function(pstart, critFn, tol,...) {
  qvals <- c(critFn(pstart[1],...), critFn(pstart[2],...))

  if (q < qvals[1]) {
    return(0)
  } else if (q > qvals[2]) {
    return(1)
  }
  
  ## do a binary search
  repeat {
    p.new <- mean(pstart)
    qval.new <- critFn(p.new,...)
    if (isTRUE(all.equal(cval.new, q))) {
      return(p.new)
    } else if (cval.new > q) {
      pstart[2] <- p.new
      qvals[2] <- qval.new
    } else {
      pstart[1] <- p.new
      qvals[1] <- qval.new
    }
    ## this is probably more accurate than we're really being, so I'm
    ## not going to worry about better refinements.
    if (abs(diff(pstart)) < 0.005) break
  }
  mean(pstart)
}

pmccracken.1 <- function(q, k, rtRatio, 
                         window = c("fixed", "recursive", "rolling"),
                         lower.tail = TRUE) {
  window <- match.arg(window)
  p <- searchAlg(c(0, 1), function(p)
                 qmccracken.1(p, k, rtRatio, window), 0.005)
  ifelse(lower.tail, p, 1 - p)
}

pmccracken.2 <- function(q, k, rtRatio, qtRatio,
                         window = c("fixed", "recursive", "rolling"),
                         lower.tail = TRUE) {
  window <- match.arg(window)
  p <- searchAlg(c(0, 1), function(p)
                 qmccracken.2(p, k, rtRatio, qtRatio, window), 0.005)
  ifelse(lower.tail, p, 1 - p)
}
