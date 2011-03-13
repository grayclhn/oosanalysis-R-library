cts <- function(x, y) {
  x <- as.ts(x)
  y <- as.ts(y)
  freq <- frequency(x)
  if (freq != frequency(y)) stop("both x and y must have the same frequency")
  ts(unname(c(x, y)), start = start(x), end = end(y), frequency = freq)
}
