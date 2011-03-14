apply.oos <- function(R, d, model,
                      window = c("rolling", "recursive", "fixed"),
                      ret = c("forecast", "error"),...) {
  window <- match.arg(window)
  ret <- match.arg(ret)
  n <- nobs(d)
  d <- as.ts(d)
  p <- time(d)
  predfn <- switch(ret, forecast = predict, error = forecast.error)
  ## note that in all of the switch statements, we're going to let
  ## 'newdata' include the training sample, and then just take the
  ## last forecast or forecast error.  This is so that dynamic models
  ## can get their regressors from the training sample.  For the same
  ## reason, we make everything a time series and use windows, instead
  ## of subsetting (for some reason, time series objects lose their
  ## time series properties after subsetting, which is kind of
  ## annoying).
  ## 
  ## Note that the "predict" methods are kind of crappy, in that they
  ## return a vector of the same length as "newdata" has observations,
  ## even when some of the observations are lost to lag strucutre,
  ## etc.
  ##
  ## As you can imagine, this code is *extremely* slow.
  lastPred <- function(startEst, endEst, s,
                       m = model(window(d, start = p[startEst], end = p[endEst]),
                         ...),...) {
    predictions <- predfn(m, newdata = window(d, start = p[startEst], end = p[s]))
    if (is.ts(predictions)) {
      window(predictions, start = p[s], end = p[s])
    } else {
      tail(predictions, 1)
    }
  }
  
  ts(unname(switch(window,
                   recursive = sapply((R+1):n, function(s) lastPred(1, s-1, s,...)),
                   rolling =   sapply((R+1):n, function(s) lastPred(s-R, s-1, s,...)),
                   fixed = {
                     m <- model(window(d, end = p[R]),...)
                     sapply((R+1):n, function(s) lastPred(1, R, s, m))
                   })), end = end(d), frequency = frequency(d))
}
