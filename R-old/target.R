setMethod("target", "oos.forecast", function(object,...) object@target)

setMethod("target", "oos.pair",
          function(object,...) target(model.null(object),...))
