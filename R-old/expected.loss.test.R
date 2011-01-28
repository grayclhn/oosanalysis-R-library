setMethod("expected.loss.test", "oos.forecast",
          function(object,...) object@expected.loss.test)

setMethod("expected.loss.test", "oos.pair",
          function(object,...)
          (expected.loss.test(model.null(object)) - expected.loss.test(model.alt(object))))
