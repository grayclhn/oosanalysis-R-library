forecast.error <- function(object, newdata,...) {
  if (missing(newdata) || is.null(newdata)) {
    stop("must supply 'newdata'")
  }
  tt <- terms(object)
  Y.new <- model.response(model.frame(tt, newdata))
  Y.hat <- predict(object, newdata,...)
  Y.new - Y.hat
}
