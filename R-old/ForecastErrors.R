ForecastErrors <- function(object,...) {
  ntarget <- length(target(object))
  lapply(forecasts(object,...), function(f)
         target(object)[seq.int(along.with = f, to = ntarget)] - f)
}
