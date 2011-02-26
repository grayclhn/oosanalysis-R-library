apply.oos <- function(R, d, model,
                      window = c("rolling", "recursive", "fixed"),
                      ret = c("forecast", "error"),...) {
  window <- match.arg(window)
  ret <- match.arg(ret)
  n <- nobs(d)
  d <- as.ts(d)
  dindex <- time(d)
  predfn <- switch(ret, forecast = predict, error = forecast.error)
  ## note that in all of the switch statements, we're going to let
  ## 'newdata' include the training sample, and then just take the
  ## last forecast or forecast error.  This is so that dynamic models
  ## can get their regressors from the training sample.  For the same
  ## reason, we make everything a time series and use windows, instead
  ## of subsetting (for some reason, time series objects lose their
  ## time series properties after subsetting, which is kind of
  ## annoying).
  switch(window,
         recursive = sapply((R+1):n, function(s)
           tail(predfn(model(window(d, end = dindex[s-1]),...),
                  newdata = window(d, end = dindex[s])), 1)),
         rolling = sapply((R+1):n, function(s)
           tail(predfn(model(window(d, start = dindex[s-R],
                                    end = dindex[s-1]),...),
                       newdata = window(d, end = dindex[s])), 1)),
         fixed = {
           m <- model(window(d, end = dindex[R]),...)
           sapply((R+1):n, function(s)
                  tail(predfn(m, newdata = window(d, end = dindex[s])), 1))
         })
}
