forecast.error <- function(object, newdata,...) {
  if (missing(newdata) || is.null(newdata)) {
    stop("must supply 'newdata'")
  }
  ## If 'newdata' is a ts object, we need to make sure that dynamic
  ## models don't mess up the indexing
  tt <- terms(object)
  Y.new <- model.response(model.frame(tt, newdata))
  if (is.ts(newdata)) {
    Y.new <- as.ts(Y.new, start = start(newdata), frequency = frequency(newdata))
  }
  Y.hat <- predict(object, newdata,...)
  Y.new - Y.hat
}
