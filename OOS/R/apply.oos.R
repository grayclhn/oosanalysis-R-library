apply.oos <- function(R, d, model, method = "recursive",...) {
  n <- nobs(d)
  switch(method,
         recursive = sapply((R+1):n, function(s)
           predict(model(d[seq.int(to = s-1),,drop=FALSE],...),
                   newdata = d[s,,drop=FALSE])),
         rolling = sapply((R+1):n, function(s)
           predict(model(d[seq.int(from = s-R, to = s-1),,drop=FALSE],...),
                   newdata = d[s,,drop=FALSE])),
         fixed = {
           m <- model(d[seq.int(to = R),,drop=FALSE],...)
           sapply((R+1):n, function(s)
                  predict(m, newdata = d[s,,drop=FALSE]))
         },
         stop("'", method, "' is an unsupported window type", sep = ""))
}
