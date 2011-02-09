## null.model: a function that takes an argument of the same type as
## 'd' and returns an object with a 'predict' method
## alt.model: a single or list of models of the same type as 'null.model'

oos.t <- function(null.model, alt.model, data, R, L,
                  window = c("rolling", "recursive", "fixed"),
                  alternative = "greater",
                  method = c("DMW", "McC07"),
                  conf.level = 0.95) {
  window <- match.arg(window)
  method <- match.arg(method)
  if (alternative != "greater")
    warning("The alternative almost always be 'greater.'  Are you sure?")

  ## If alt.model is a list, we want to return a list; otherwise we're
  ## going to return a single statistic.  returnList is the boolean
  ## that indicates whether or not we should return a list.
  returnList <- is.list(alt.model)
  if (!returnList) alt.model <- list(a = alt.model)

  ## forecast errors for the benchmark model
  null.errors <- apply.oos(R, data, null.model, window, ret = "error")
  P1 <- length(null.errors)
  ## calculate the forecast errors for each of the alternative models
  ## difference between the benchmark
  loss.diff <- lapply(alt.model, function(alt) L(null.errors) -
                      L(apply.oos(R, data, alt, window, ret = "error")))

  df <- c(R, P1)
  names(df) <- c("R", "P")
  methodText <- paste("One-sample OOS t-test, ", window,
                      " window (", method, ")", sep = "")
  data.name = deparse(substitute(data))  

  if (method == "DMW") {
    ## use the normal approximation for the DMW test.
    pfn <- function(x,...) pt(x, df = P1-1, ...)
    qfn <- function(x,...) qt(x, df = P1-1, ...)
  } else stop("That method is not yet supported")

  ## taken basically from the t-test source code
  if (alternative == "less") {
    pval <- function(tstat) pfn(tstat)
    cint <- function(mx, std) {
      ret <- mx + std * c(-Inf, qfn(conf.level))
      attr(ret, "conf.level") <- conf.level
      ret
    }
  } else if (alternative == "greater") {
    pval <- function(tstat) pfn(tstat, lower.tail = FALSE)
    cint <- function(mx, std) {
      ret <- mx + std * c(- qfn(conf.level), Inf)
      attr(ret, "conf.level") <- conf.level
      ret
    }
  } else if (alternative == "two.sided") {
    pval <- function(tstat) 2 * pfn(- abs(tstat))
    cint <- function(mx, std) {
      ret <- qfn(0.5 * (1 + conf.level))
      ret <- mx + std * c(-ret, ret)
      attr(ret, "conf.level") <- conf.level
      ret
    }
  } else {
    stop("Invalid choice for 'alternative'.")
  }
    
  ## actually calculate the oos t-statistic for each sequence of loss
  ## differences.
  tstats <- lapply(seq_along(loss.diff), function(x) {
    mx <- mean(loss.diff[[x]])
    estimate <- mx
    names(estimate) <- "OOS Avg. 1"

    sdx <- sd(loss.diff[[x]])
    ooststat <- sqrt(P1) * mx / sdx
    names(ooststat) <- "oos-t"
    std <- sdx / sqrt(P1)
    
    rval <- list(statistic = ooststat, parameter = df, p.value = pval(mx / std),
                 conf.int = cint(mx, std), estimate = estimate,
                 null.value = 0, alternative = alternative,
                 method = methodText, data.name = data.name)
    class(rval) <- "htest"
    rval})
  
  if (!returnList) tstats <- tstats[[1]]
  tstats
}

oos.t2 <- function(null.model, alt.model, data, data2 = NULL,
                   window = c("rolling", "recursive", "fixed"),
                   alternative = c("two.sided", "less", "greater"),
                   method = c("DMW", "McC07"),
                   L, R, conf.level = 0.95) {
  window <- match.arg(window)
  alternative <- match.arg(alternative)
  method <- match.arg(method)

  returnList <- is.list(alt.model)
  if (!returnList) alt.model <- list(a = alt.model)
  
  null.errors <- apply.oos(R, data, null.model, window, ret = "error")
  P1 <- length(null.errors)
  loss.diff <- lapply(alt.model, function(alt) L(null.errors) - L(apply.oos(R, data, alt, window, ret = "error")))

  if (!is.null(data2)) {
    dindex <- switch(window, rolling = seq.int(to = nobs(data), length = R), seq.int(length = nobs(data)))
    R2 <- length(dindex)
    d2 <- rbind(data[dindex,,drop=FALSE], data2)
    null.errors2 <- apply.oos(R2, d2, null.model, window, ret = "error")
    P2 <- length(null.errors2)
    loss.diff2 <- lapply(alt.model, function(alt) L(null.errors2) - L(apply.oos(R2, d2, alt, window, ret = "error")))
    data.name = c(deparse(substitute(data)), deparse(substitute(data2)))
    methodText = paste("Two-sample OOS t-test, ", window, " window (", method, ")", sep = "")
  } else {
    data.name = deparse(substitute(data))  
    methodText = paste("One-sample OOS t-test, ", window, " window (", method, ")", sep = "")
    P2 <- Inf
  }
    
  df <- c(R, P1, P2)
  names(df) <- c("R", "P1", "P2")

  if (method == "DMW") {
    pfn <- function(x,...) pnorm(x,...)
    qfn <- function(x,...) qnorm(x,...)
  } else stop("That method is not yet supported")

  ## taken basically from the t-test source code
  if (alternative == "less") {
    pval <- function(tstat) pfn(tstat)
    cint <- function(mx, std) {
      ret <- mx + std * c(-Inf, qfn(conf.level))
      attr(ret, "conf.level") <- conf.level
      ret
    }
  } else if (alternative == "greater") {
    pval <- function(tstat) pfn(tstat, lower.tail = FALSE)
    cint <- function(mx, std) {
      ret <- mx + std * c(- qfn(conf.level), Inf)
      attr(ret, "conf.level") <- conf.level
      ret
    }
  } else {
    pval <- function(tstat) 2 * pfn(- abs(tstat))
    cint <- function(mx, std) {
      ret <- qfn(0.5 * (1 + conf.level))
      ret <- mx + std * c(-ret, ret)
      attr(ret, "conf.level") <- conf.level
      ret
    }
  }
    
  ## actually calculate the oos t-statistics
  tstats <- lapply(seq_along(loss.diff), function(x) {
    mx <- mean(loss.diff[[x]])
    if (!is.null(data2)) {
      my <- mean(loss.diff2[[x]])
      estimate <- c(mx, my)
      names(estimate) <- c("OOS Avg. 1", "OOS Avg. 2")
    } else {
      estimate <- mx
      names(estimate) <- "OOS Avg. 1"
    }

    sdx <- sd(loss.diff[[x]])
    ooststat <- sqrt(P1) * mx / sdx
    names(ooststat) <- "oos-t"
    std <- sdx * sqrt(1/P1 + 1/P2)
    
    rval <- list(statistic = ooststat, parameter = df, p.value = pval((mx - my) / std),
                 conf.int = cint(mx, std), estimate = estimate,
                 null.value = 0, alternative = alternative,
                 method = methodText, data.name = data.name)
    class(rval) <- "htest"
    rval})
  
  if (!returnList) tstats <- tstats[[1]]
  tstats
}
