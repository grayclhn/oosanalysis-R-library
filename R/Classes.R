## Copyright (C) 2011-2015 Gray Calhoun; MIT license

CT <- function(model) {
    if (!HasMethod(model, "predict"))
      stop("'model' must have a 'predict' method.")
  class(model) <- c("CT", class(model))
  model
}
predict.CT <- function(object, newdata,...) {
  predictions <- NextMethod(object, newdata,...)
  ifelse(predictions > 0, predictions, 0)
}
Aggregate <- function(model.list, fn) {
  if (!is.function(fn)) stop("'fn' must be a function.")
  if (!all(sapply(model.list, function(m) HasMethod(m, "predict"))))
    stop("Each element of 'model.list' must have a 'predict' method defined.")
  class(model.list) <- "Aggregate"
  attr(model.list, "Aggregator") <- fn
  model.list
}
predict.Aggregate <- function(object, newdata,...) {
  arguments <- list(...)
  arguments$newdata <- newdata
  forecasts <- sapply(object, function(model) 
                      do.call("predict", c(list(object = model), arguments)))
  if (nrow(newdata) > 1) {
    return(apply(forecasts, 1, attr(object, "Aggregator")))
  } else {
    return(attr(object, "Aggregator")(forecasts))
  }
}
HasMethod <- function(object, method.name) {
  sapply(method.name, function(name) {
    listed.methods <- methods(name)
    any(sapply(class(object), function(eachclass)
               length(grep(eachclass, listed.methods))) > 0)
  })
}
