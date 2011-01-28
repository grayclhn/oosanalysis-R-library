setMethod("nreg", "oos.forecast",
          function(object,...) object@nreg)

setMethod("nreg", "oos.pair",
          function(object) c(nreg(model.null(object)), nreg(model.alt(object))))
