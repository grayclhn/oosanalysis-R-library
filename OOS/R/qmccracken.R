interpolate <- function(at, coords, values) {
  coords <- as.data.frame(coords)
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

mccIndex <- function(x, maxIndex, offset = 0) {
  xI <- 100 * x + offset
  maxIndex <- maxIndex + offset
  ## using ceiling(xI-1) instead of floor(xI) is intentional; if x is
  ## an integer, floor(xI) returns the same thing as ceiling(xI), but
  ## ceiling(xI-1) returns xI-1.
  xvec <- c(ceiling(xI - 1), ceiling(xI))
  if (any(xvec < 1)) {
    xvec <- c(1,2)
    xI <- 1
  } else if (any(xvec > maxIndex)) {
    xvec <- c(maxIndex- 1, maxIndex)
    xI <- maxIndex 
  }
  c(xI, xvec)
}

qmccracken.1 <- function(p, k, R, P,
                         window = c("fixed", "recursive", "rolling")) {
  window <- match.arg(window)
  ## we've tabluated values for p = 0, 0.01, ..., 1.00 and for R/(R+P)
  ## = 0.01,...,0.99 in the array mccArray1, accessed in the order
  ## 
  ## (100 * R/R+P, 100 * p + 1, k)
  p  <- mccIndex(p, 100, 1)
  rt <- mccIndex(R / (R+P), 99)

  coords <- expand.grid(p = p[-1], rt = rt[-1])
  values <- mapply(function(p, rt) mccArray1[[window]][rt, p, k],
                   p = coords$p, rt = coords$rt)
  interpolate(c(p[1], rt[1]), coords, values)
}

qmccracken.2 <- function(p, k, R, P1, P2,
                         window = c("fixed", "recursive", "rolling")) {
  window <- match.arg(window)
  ## we've tabluated values for p = 0, 0.01, ..., 1.00 and for
  ## R/(R+P1+P2) and P1/(R+P1+P2) = 0.01,...,0.99 in the array
  ## mccArray2, accessed in the order
  ##
  ## (100 * R/(R+P1+P2), 100 * P1/(R+P1+P2), 100 * p + 1, k)
  p <- mccIndex(p, 100, 1)
  rn <- mccIndex(R / (R+P1+P2), 99)
  pn <- mccIndex(P1 / (R+P1+P2), 99)

  coords <- expand.grid(p = p[-1], rn = rn[-1], pn = pn[-1])
  values <- mapply(function(p, rn, pn) mccArray2[[window]][rn, pn, p, k],
                   p = coords$p, rn = coords$rn, pn = coords$pn)
  interpolate(c(p[1], rn[1], pn[1]), coords, values)
}

searchAlg <- function(target, pstart, critFn, maxIterations = 200,
                      tol = .Machine$double.eps^0.25,...) {
  qvals <- c(critFn(pstart[1],...), critFn(pstart[2],...))

  if (target < qvals[1]) {
    return(0)
  } else if (target > qvals[2]) {
    return(1)
  }

  nIterations <- 0
  ## do a binary search
  repeat {
    nIterations <- nIterations + 1
    p.new <- mean(pstart)
    qval.new <- critFn(p.new,...)
    if (isTRUE(all.equal(qval.new, target))) {
      return(p.new)
    } else if (qval.new > target) {
      pstart[2] <- p.new
      qvals[2] <- qval.new
    } else {
      pstart[1] <- p.new
      qvals[1] <- qval.new
    }
    ## this is probably more accurate than we're really being, so I'm
    ## not going to worry about better refinements. (ultimately, we
    ## have a critical values from a monte carlo and we're
    ## interpolating them)
    if (abs(diff(pstart)) < tol) {
      ret <- mean(pstart)
      attr(ret, "code") <- 0
      return(ret)
    } else if (nIterations > maxIterations) {
      ret <- mean(pstart)
      attr(ret, "code") <- 1
      return(ret)
    }
  }
}

pmccracken.1 <- function(q, k, R, P,
                         window = c("fixed", "recursive", "rolling"),
                         lower.tail = TRUE,...) {
  window <- match.arg(window)
  p <- searchAlg(q, c(0, 1),
                 function(p) qmccracken.1(p, k, R, P, window),...)
  ifelse(lower.tail, p, 1 - p)
}

pmccracken.2 <- function(q, k, R, P1, P2,
                         window = c("fixed", "recursive", "rolling"),
                         lower.tail = TRUE,...) {
  window <- match.arg(window)
  p <- searchAlg(q, c(0, 1), function(p)
                 qmccracken.2(p, k, R, P1, P2, window),...)
  ifelse(lower.tail, p, 1 - p)
}
