lagmatrix <- function(x, L) {
  x <- as.ts(x)
  xmat <- do.call(cbind, lapply(seq(length = L), function(s) lag(x, -s)))
  if (!is.matrix(xmat)) dim(xmat) <- c(length(xmat), 1)
  if (is.null(colnames(x))) {
    colnames(xmat) <- rep(paste("L", 1:L, sep = ""),
                          each = ncol(x))
  } else {
    colnames(xmat) <- c(sapply(1:L, function(i)
                        paste(colnames(x), "L", i, sep = "")))
  }
  window(xmat, start = c(L,0)+start(x), end = end(x))
}
