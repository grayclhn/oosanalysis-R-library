window.data.frame <- function(x, start = NULL, end = NULL, extend = FALSE, ...) {
  n <- nrow(x)
  k <- ncol(x)
  if (is.null(start)) start <- 1
  if (is.null(end)) end <- n
  if (extend & end > n) {
    xextend <- as.data.frame(matrix(NA, end - n, k))
    colnames(xextend) <- colnames(x)
    return(rbind(x, xextend)[seq.int(start,end),,drop=FALSE])
  } else if (end > n) {
    warning("'end' value not changed")
    end <- n
  }
  x[seq.int(start, end),,drop=FALSE]
}
