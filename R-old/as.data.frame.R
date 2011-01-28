as.data.frame.oos.pair <- function(x,...) {
  data.frame(n     = nobs(x),
             ntest = ntest(x),
             kNull = nreg(model.null(x)),
             kAlt  = nreg(model.alt(x)),
             norm  = bNorm(x),
             elossFuture = expected.loss.future(x),
             elossFutureNull = expected.loss.future(model.null(x)),
             elossFutureAlt  = expected.loss.future(model.alt(x)),
             elossTest = expected.loss.test(x),
             elossTestNull = expected.loss.test(model.null(x)),
             elossTestAlt  = expected.loss.test(model.alt(x)),
             ...)
}

as.data.frame.oos.forecast <- function(x,...) {
  data.frame(ntest = ntest(x),
             nreg  = nreg(x),
             ...)
}
