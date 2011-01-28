setMethod("ntest", "oos.forecast",
          function(object,...) sapply(forecasts(object,...),
                                      function(f) length(f)))

setMethod("ntest", "oos.pair",
          function(object,...) ntest(model.null(object),...))
