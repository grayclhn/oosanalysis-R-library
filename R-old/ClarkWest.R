ClarkWest <- function(x,...) {
  null.model <- model.null(x)
  alt.model  <- model.alt(x)
  unname(sapply(mapply(function(f.null, e.null, f.alt, e.alt)
                       e.null^2 - e.alt^2 + (f.null - f.alt)^2,
                       f.null = forecasts(null.model),
                       f.alt = forecasts(alt.model),
                       e.null = ForecastErrors(null.model),
                       e.alt = ForecastErrors(alt.model)),
                function(x) t.test(x,...)$statistic))
}

