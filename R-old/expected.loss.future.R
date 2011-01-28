setMethod("expected.loss.future", "oos.forecast",
          function(object,...) object@expected.loss.future)

setMethod("expected.loss.future", "oos.pair",
          function(object,...)
          (expected.loss.future(model.null(object)) - expected.loss.future(model.alt(object))))
