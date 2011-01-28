setMethod("CenteredPValue", "oos.forecast",
          function(object,...)
          as.data.frame(object,
                        p.value = mapply(function(e, mu)
                          t.test(e^2, mu = mu, ...)$p.value,
                          e = ForecastErrors(object),
                          mu = expected.loss.test(object)),...))

## returns the p-value for a t-test that the difference in the two
## models' mse is equal to its known value.  Note that the default is
## a two-sided test; that can be changed as usual for the t-test.
setMethod("CenteredPValue", "oos.pair",
          function(object,...)
          as.data.frame(object,
                        p.value = mapply(function(e1, e2, mu)
                          t.test(e1^2 - e2^2, mu = mu, ...)$p.value,
                          e1 = ForecastErrors(model.null(object)),
                          e2 = ForecastErrors(model.alt(object)),
                          mu = expected.loss.test(object)),...))
