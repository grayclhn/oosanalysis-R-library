apply.oos <- function(R, d, model, window = c("rolling", "recursive", "fixed"),
                      ret = c("forecast", "error"),...) {
  window <- match.arg(window)
  ret <- match.arg(ret)
  n <- nobs(d)
  predfn <- switch(ret, forecast = predict, error = forecast.error)
  switch(window,
         recursive = sapply((R+1):n, function(s)
           predfn(model(d[seq.int(to = s-1),,drop=FALSE],...),
                  newdata = d[s,,drop=FALSE])),
         rolling = sapply((R+1):n, function(s)
           predfn(model(d[seq.int(from = s-R, to = s-1),,drop=FALSE],...),
                  newdata = d[s,,drop=FALSE])),
         fixed = {
           m <- model(d[seq.int(to = R),,drop=FALSE],...)
           sapply((R+1):n, function(s)
                  predfn(m, newdata = d[s,,drop=FALSE]))
         })
}
