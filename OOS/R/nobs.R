setGeneric("nobs", function(x,...) standardGeneric("nobs"))

setMethod("nobs", "data.frame", function(x) nrow(x))
setMethod("nobs", "zoo", function(x) nrow(x))
