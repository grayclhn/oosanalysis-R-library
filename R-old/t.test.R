t.test.oos.forecast <- function(x, mu = "future",
                                transform = "none",...) {
  if (transform == "rmse") {
    location <- function(L) sqrt(L)
    vcvscale <- function(L) 0.5 / sqrt(L)
  } else if (transform == "log") {
    location <- function(L) log(L)
    vcvscale <- function(L) 1/L
  } else {
    location <- function(L) L
    vcvscale <- function(L) 1
  }

  if (mu == "future") {
    comparison <- location(expected.loss.future(x))
  } else if (mu == "test") {
    comparison <- location(expected.loss.test(x))
  } else stop("Invalid value for 'mu'")
  
  unname(mapply(function(e, mu) {
    L <- mean(e^2)
    sqrt(length(e)) * (location(L) - mu) / sqrt(var(e^2) * vcvscale(L)^2)
  }, e = ForecastErrors(x), mu = comparison))
}

t.test.oos.pair <- function(x, mu = "unknown",
                            transform = "difference",...) {

  if (transform == "difference") {
    location <- function(L1, L2)  L1 - L2
    vcvVector <- function(L1, L2) c(1, -1)
  } else if (transform == "ratio") {
    location <- function(L1, L2) L1/L2
    vcvVector <- function(L1, L2) c(1/L2, - L1/L2^2)
  } else if (transform == "rmse difference") {
    location <- function(L1, L2) sqrt(L1) - sqrt(L2)
    vcvVector <- function(L1, L2) c(0.5 / sqrt(L1), - 0.5 / sqrt(L2))
  } else if (transform == "rmse ratio") {
    location <- function(L1, L2) sqrt(L1)/sqrt(L2)
    vcvVector <- function(L1, L2)
      c(0.5 / sqrt(L1 * L2), - 0.5 * sqrt(L1) * L2^(-1.5))
  } else if (transform == "log") {
    location <- function(L1, L2) log(L1) - log(L2)
    vcvVector <- function(L1, L2) c(1/L1, - 1/L2)
  } else stop("Invalid value for transform")

  if (mu == "future") {
    comparison <- location(expected.loss.future(model.null(x)),
                           expected.loss.future(model.alt(x)))
  } else if (mu == "test") {
    comparison <- location(expected.loss.test(model.null(x)),
                      expected.loss.test(model.alt(x)))
  } else if (mu == "unknown") {
    comparison <- location(1,1)
  } else stop("Invalid choice of 'mu'")

  unname(mapply(function(e1, e2, mu) {
    L1 <- mean(e1^2)
    L2 <- mean(e2^2)
    vcvvec <- vcvVector(L1, L2)
    variance <- crossprod(vcvvec, crossprod(var(cbind(e1^2, e2^2)),
                                            vcvvec))
    drop(sqrt(length(e1)) * (location(L1, L2) - mu) / sqrt(variance))
  }, e1 = ForecastErrors(model.null(x)),
     e2 = ForecastErrors(model.alt(x)),
     mu = comparison))
}
