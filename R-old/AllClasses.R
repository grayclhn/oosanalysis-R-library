setClass("oos.forecast",
         representation(expected.loss.test   = "numeric",
                        expected.loss.future = "numeric",
                        forecasts            = "list",
                        target               = "numeric",
                        nreg                 = "integer"))

forecasts <- function(object,...) object@forecasts

setClass("oos.pair",
         representation(model.null = "oos.forecast",
                        model.alt  = "oos.forecast",
                        f.test     = "numeric",
                        b.true     = "numeric",
                        nobs       = "integer"))

model.null <- function(object,...) object@model.null
model.alt <- function(object,...) object@model.alt
f.test <- function(object,...) object@f.test
nobs <- function(object,...) object@nobs
bNorm <- function(object,...) sqrt(crossprod(object@b.true))

setValidity("oos.pair", function(object) {
  error.text <- NULL
  
  AddError <- function(x)
    ## Convenience function so that we can accumulate error messages
    ## without halting execution
    paste(error.text, x, sep = "\n")
  CheckThis <- function(x)
    ## Convenience wrapper for tryCatch; we just want to accumulate the
    ## error messages if our test code doesn't run
    tryCatch(isTRUE(x), error = function(e) AddError(e$message))
  
  CheckThis(length(f.test(object)) == 1)
  CheckThis(length(nobs(object)) == 1)
  CheckThis(all.equal(ntest(model.null(object)),
                            ntest(model.alt(object))))
  CheckThis(all.equal(target(model.null(object)),
                      target(model.alt(object))))
  
  if (is.null(error.text))
    return(TRUE)
  else
    return(error.text)
})
