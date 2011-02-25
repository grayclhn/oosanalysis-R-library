htest.start <- function(null.value, null.text, parameter, parameter.text,
                        method, data.name, alternative) {
  rval <- list(null.value = null.value, parameter = parameter,
               method = method, data.name = data.name, alternative = alternative)
  names(rval$null.value) <- null.text
  names(rval$parameter) <- parameter.text
  
  class(rval) <- "htest"
  rval
}

htest.finish <- function(rval, estimate, estimate.text, statistic,
                         statistic.text, p.value, conf.int, conf.int.text) {
  rval$estimate <- estimate
  names(rval$estimate) <- estimate.text
  rval$statistic <- statistic
  names(rval$statistic) <- statistic.text
  rval$p.value <- p.value
  rval$conf.int <- conf.int
  attr(rval$conf.int, "conf.level") <- conf.int.text
  rval
}

## null.model: a function that takes an argument of the same type as
## 'd' and returns an object with a 'predict' method
## alt.model: a single or list of models of the same type as 'null.model'

oos.t <- function(null.model, alt.model, data, R,
                  L = function(x) x^2,
                  window = c("rolling", "recursive", "fixed"),
                  method = c("DMW", "McC07"), alternative = "greater",
                  conf.level = 0.95) {
  window <- match.arg(window)
  method <- match.arg(method)
  if (alternative != "greater")
    warning("The alternative is almost always 'greater.'  Are you sure?")

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

  ## start building the 'htest' object we'll return
  rval <- htest.start("E(L.alt)", "E(L.null)", c(R, P1), c("R", "P"),
                      paste("One-sample OOS t-test, ", window, " window (", method, ")", sep = ""),
                      deparse(substitute(data)), alternative)

  if (method == "DMW") {
    ## use the t approximation for the DMW test so that the numbers
    ## are consistent with t.test.
    pfn <- function(x,...) pt(x, df = P1-1, ...)
    qfn <- function(x,...) qt(x, df = P1-1, ...)
  } else stop("That method is not yet supported")

  ## It's convenient to define different functions to calculate the
  ## p.value and the confidence intervals outside the coming 'lapply',
  ## so we only need to do it once.
  if (alternative == "less") {
    pval <- function(tstat) pfn(tstat)
    cint <- function(mx, std)  mx + std * c(-Inf, qfn(conf.level))
  } else if (alternative == "greater") {
    pval <- function(tstat) pfn(tstat, lower.tail = FALSE)
    cint <- function(mx, std) mx + std * c(- qfn(conf.level), Inf)
  } else if (alternative == "two.sided") {
    pval <- function(tstat) 2 * pfn(- abs(tstat))
    cint <- function(mx, std) mx + std * c(-1, 1) * qfn(0.5 * (1 + conf.level))
  } else {
    stop("Invalid choice for 'alternative'.")
  }
    
  ## actually calculate the oos t-statistic for each sequence of loss
  ## differences.
  tstats <- lapply(seq_along(loss.diff), function(x) {
    mx <- mean(loss.diff[[x]])
    std <- sd(loss.diff[[x]]) / sqrt(P1)

    htest.finish(rval, mx, "OOS Avg. 1", mx/std, "oos-t",
                 pval(mx/std), cint(mx, std), conf.level)
    })
  
  if (!returnList) tstats <- tstats[[1]]
  tstats
}

oos.t2 <- function(null.model, alt.model, data, data2 = NULL,
                   R, P2 = nobs(data2), L = function(x) x^2,
                   window = c("rolling", "recursive", "fixed"),
                   method = c("DMW", "McC07"), alternative = "greater",
                   conf.level = 0.95) {
  window <- match.arg(window)
  method <- match.arg(method)
  if (alternative != "greater")
    warning("The alternative is almost always 'greater.'  Are you sure?")

  returnList <- is.list(alt.model)
  if (!returnList) alt.model <- list(a = alt.model)
  
  null.errors <- apply.oos(R, data, null.model, window, ret = "error")
  P1 <- length(null.errors)
  loss.diff <- lapply(alt.model, function(alt) L(null.errors) - L(apply.oos(R, data, alt, window, ret = "error")))

  ## The '&&' is important (instead of '&') so that we don't evaluate
  ## nobs(data2) if data2 is NULL.
  if (!is.null(data2) && P2 <= nobs(data2)) {
    ## if data2 is supplied, we calculate the out-of-sample loss over
    ## the second oos period.  We have to handle the rolling window
    ## differently than the fixed or recursive window, since R doesn't
    ## change for the rolling window.
    cboth <- intersect(colnames(data), colnames(data2))
    if (window == "rolling") {
      dfull <- rbind(subset(data[seq.int(to=nobs(data), length=R),,drop=FALSE], select=cboth),
                     subset(data2[seq.int(to=P2),,drop=FALSE], select=cboth))
      R2 <- R
    } else {
      dfull <- rbind(subset(data, select = cboth), subset(data2[seq.int(to=P2),,drop=FALSE], select = cboth))
      R2 <- nobs(data)
    }
    
    null.errors2 <- apply.oos(R2, dfull, null.model, window, ret = "error")
    loss.diff2 <- lapply(alt.model, function(alt) L(null.errors2)
                         - L(apply.oos(R2, dfull, alt, window, ret = "error")))
    data.name = c(deparse(substitute(data)), deparse(substitute(data2)))
    methodText = paste("Two-sample OOS t-test, ", window, " window (", method, ")", sep = "")
  } else {
    data.name = deparse(substitute(data))  
    methodText = paste("One-sample OOS t-test, ", window, " window (", method, ")", sep = "")
  }

  rval <- htest.start("L.alt-bar", "L.null-bar", c(R, P1, P2), c("R", "P1", "P2"),
                      methodText, data.name, alternative)

  if (method == "DMW") {
    ## I'm going to use the t-distribution for intervals, etc, for
    ## consistency with oos.t (I want them to give the same results
    ## when P2 = Inf).  Obviously, this statistic is coming from
    ## asymptotic normality and the finite sample distribution is not
    ## a t distribution.  But this will be a little more conservative
    ## than the normal distribution, and if it affects your results
    ## then they're pretty sensitive anyway and you should think some
    ## more about whether the asymptotics are going to give reliable
    ## approximations.
    pfn <- function(x,...) pt(x, df = P1-1,...)
    qfn <- function(x,...) qt(x, df = P1-1,...)
  } else stop("That method is not yet supported")

  if (alternative == "less") {
    pval <- function(tstat) pfn(tstat)
    cint <- function(mx, std) mx + std * c(-Inf, qfn(conf.level))
  } else if (alternative == "greater") {
    pval <- function(tstat) pfn(tstat, lower.tail = FALSE)
    cint <- function(mx, std) mx + std * c(- qfn(conf.level), Inf)
  } else {
    pval <- function(tstat) 2 * pfn(- abs(tstat))
    cint <- function(mx, std) mx + std * c(-1, 1) * qfn(0.5 * (1 + conf.level))
  }
    
  ## actually calculate the oos t-statistics
  tstats <- lapply(seq_along(loss.diff), function(x) {
    mx <- mean(loss.diff[[x]])
    std <- sd(loss.diff[[x]]) * sqrt(1/P1 + 1/P2)
    
    if (!is.null(data2)) {
      my <- mean(loss.diff2[[x]])
      htest.finish(rval, c(mx, my), c("OOS Avg. 1", "OOS Avg. 2"), (mx-my) / std,
                   "oos-t2", pval((mx-my)/std), cint(mx, std), conf.level)
    } else {
      htest.finish(rval, mx, "OOS Avg. 1", mx/std, "oos-t2", pval(mx/std), cint(mx, std), conf.level)
    }})
  if (!returnList) tstats <- tstats[[1]]
  tstats
}
