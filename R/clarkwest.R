## Copyright (C) 2011-2015 Gray Calhoun; MIT license

clarkwest <- function(null, alt, dataset, R, vcv = var,
                      window = c("rolling", "recursive", "fixed"))
  clarkwest_calculation(
    target        = extract_target(null, dataset[-seq_len(R),,drop = FALSE]),
    null.forecast = recursive_forecasts(null, dataset, R, window),
    alt.forecast  = recursive_forecasts(alt, dataset, R, window),
    vcv)

clarkwest_calculation <- function(target, null.forecast, 
                                  alt.forecast, vcv) {
  P <- length(target)
  oos.sequence <- {(target - null.forecast)^2 - 
                   (target - alt.forecast)^2 + 
                   (null.forecast - alt.forecast)^2}
  mu <- mean(oos.sequence)
  avar <- vcv(oos.sequence)
  return(list(mu = mu, avar = avar, 
              pvalue = pnorm(sqrt(P) * mu, 0, sqrt(avar), FALSE)))
}
