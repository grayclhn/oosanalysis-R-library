sortdf <- function(x, cols) x[do.call("order", x[,cols,drop=FALSE]),,drop=FALSE]
