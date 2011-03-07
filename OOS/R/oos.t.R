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

## 'alternative' is the loss of the benchmark model relative to the
## alternative.  So the usual alternative is going to be that the loss
## of the null model is "greater" than that of the alternative.

oos.t <- function(null.model, alt.model, data, R,
                  L = function(x) x^2,
                  window = c("rolling", "recursive", "fixed"),
                  method = c("DMW", "Mcc:07"), alternative = "greater",
                  conf.level = 0.95) {
  window <- match.arg(window)
  method <- match.arg(method)
  if (alternative != "greater")
    warning("The alternative is almost always 'greater.'  Are you sure?")

  if (method == "DMW") {
    ## use the t approximation for the DMW test so that the numbers
    ## are consistent with t.test.
    pfn <- function(x, k,...) pt(x, df = P-1,...)
    qfn <- function(x, k,...) qt(x, df = P-1,...)
  } else {
    pfn <- function(x, k,...) pmccracken.1(x, k, R/(R+P), window,...)
    qfn <- function(x, k,...) qmccracken.1(x, k, R/(R+P), window,...)
    ## override the supplied loss function
    if (!missing(L))
      warning("McCracken's (2007) test requires that the loss function be MSE, so your argument will be overridden")
    L <- function(x) x^2
  }

  ## If alt.model is a list, we want to return a list; otherwise we're
  ## going to return a single statistic.  returnList is the boolean
  ## that indicates whether or not we should return a list.
  returnList <- is.list(alt.model)
  if (!returnList) alt.model <- list(a = alt.model)
  if (R >= nobs(data)) stop("'R' is larger than the dataset")
  data.name <- deparse(substitute(data))
  data <- as.ts(data)

  ## forecast errors for the benchmark model
  null.errors <- apply.oos(R, data, null.model, window, ret = "error")
  P <- length(null.errors)

  ## start building the 'htest' object we'll return
  rval <- htest.start("E(L.alt)", "E(L.null)", c(R, P), c("R", "P"),
                      paste("One-sample OOS t-test, ", window,
                            " window (", method, ")", sep = ""),
                      data.name, alternative)

  ## It's convenient to define different functions to calculate the
  ## p.value and the confidence intervals outside the coming 'lapply',
  ## so we only need to do it once.
  if (alternative == "greater") {
    pval <- function(tstat,...) pfn(tstat, lower.tail = FALSE,...)
    cint <- function(mx, std,...) mx + std * c(- qfn(conf.level,...), Inf)
  } else if (alternative == "two.sided") {
    pval <- function(tstat,...) 2 * pfn(- abs(tstat),...)
    cint <- function(mx, std,...) mx + std * c(qfn(conf.level/2,...),
                                               qfn(0.5 * (1 + conf.level),...))
  } else if (alternative == "less") {
    pval <- function(tstat,...) pfn(tstat,...)
    cint <- function(mx, std,...)  mx + std * c(-Inf, qfn(conf.level,...))
  } else {
    stop("Invalid choice for 'alternative'.")
  }
    
  ## actually calculate the oos t-statistic for each sequence of loss
  ## differences.
  k.null <- ncol(model.matrix(null.model(data)))
  tstats <- lapply(alt.model, function(alt) {
    loss.diff <- (L(null.errors) -
                  L(apply.oos(R, data, alt, window, ret = "error")))
    kdiff <- ncol(model.matrix(alt(data))) - k.null
    mx <- mean(loss.diff)
    std <- sd(loss.diff) / sqrt(P)

    htest.finish(rval, mx, "OOS Avg. 1", mx/std, "oos-t",
                 pval(mx/std, k = kdiff), cint(mx, std, k = kdiff),
                 conf.level)
  })
  
  if (!returnList) tstats <- tstats[[1]]
  tstats
}

## 'alternative' is the loss of the benchmark model relative to the
## alternative.  So the usual alternative is going to be that the loss
## of the null model is "greater" than that of the alternative.
oos.t2 <- function(null.model, alt.model, data, data2 = NULL,
                   R, P2 = nobs(data2), L = function(x) x^2,
                   window = c("rolling", "recursive", "fixed"),
                   method = c("DMW", "Mcc:07"), alternative = "greater",
                   conf.level = 0.95) {
  window <- match.arg(window)
  method <- match.arg(method)
  if (alternative != "greater")
    warning("The alternative is almost always 'greater.'  Are you sure?")

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
    pfn <- function(x, k,...) pt(x, df = P1-1,...)
    qfn <- function(x, k,...) qt(x, df = P1-1,...)
  } else {
    pfn <- function(x, k,...) pmccracken.2(x, k, R/(R+P1), P2/(R+P1), window,...)
    qfn <- function(x, k,...) qmccracken.2(x, k, R/(R+P1), P2/(R+P1), window,...)
    ## override the supplied loss function
    if (!missing(L))
      warning("McCracken's (2007) test requires that the loss function be MSE, so your argument will be overridden")
    L <- function(x) x^2
  }
  
  returnList <- is.list(alt.model)
  if (!returnList) alt.model <- list(a = alt.model)
  if (R >= nobs(data)) stop("'R' is larger than the dataset")
  data.name <- deparse(substitute(data))
  data <- as.ts(data)
  
  null.errors <- apply.oos(R, data, null.model, window, ret = "error")
  P1 <- length(null.errors)
  
  ## The '&&' is important (instead of '&') so that we don't evaluate
  ## nobs(data2) if data2 is NULL.
  if (!is.null(data2) && P2 <= nobs(data2)) {
    data.name <- c(data.name, deparse(substitute(data2)))
    data2 <- as.ts(data2)
    ## if data2 is supplied, we calculate the out-of-sample loss over
    ## the second oos period.  We have to handle the rolling window
    ## differently than the fixed or recursive window, since R doesn't
    ## change for the rolling window.  This is kind of a bitch
    ## notationally if you need to allow for dynamic models.
    dindex <- time(data)
    cboth <- intersect(colnames(data), colnames(data2))
    dfull <- ts(rbind(data[, cboth, drop=FALSE],
                      window(data2[, cboth, drop=FALSE],
                             end = time(data2)[P2])),
                start = dindex[1], frequency = frequency(data))
    if (window == "rolling") {
      dfull <- window(dfull, start = dindex[nobs(data)-R+1])
      R2 <- R
    } else {
      R2 <- nobs(data)
    }
    methodText <- paste("Two-sample OOS t-test, ", window, " window (", method, ")", sep = "")
    null.errors2 <- apply.oos(R2, dfull, null.model, window, ret = "error")
  } else {
    methodText <- paste("One-sample OOS t-test, ", window, " window (", method, ")", sep = "")
  }

  rval <- htest.start("L.alt-bar", "L.null-bar", c(R, P1, P2), c("R", "P1", "P2"),
                      methodText, data.name, alternative)

  if (alternative == "greater") {
    pval <- function(tstat,...) pfn(tstat, lower.tail = FALSE,...)
    cint <- function(mx, std,...) mx + std * c(- qfn(conf.level,...), Inf)
  } else if (alternative == "less") {
    pval <- function(tstat,...) pfn(tstat,...)
    cint <- function(mx, std,...) mx + std * c(-Inf, qfn(conf.level,...))
  } else if (alternative == "two.sided") {
    pval <- function(tstat,...) 2 * pfn(- abs(tstat),...)
    cint <- function(mx, std,...) mx + std * c(qfn(conf.level/2,...),
                                               qfn(0.5 * (1 + conf.level),...))
  } else stop("Invalid choice for alternative")

  k.null <- ncol(model.matrix(null.model(data)))
  ## actually calculate the oos t-statistics
  tstats <- lapply(alt.model, function(alt) {
    loss.diff <- (L(null.errors)
                  - L(apply.oos(R, data, alt, window, ret = "error")))
    
    mx <- mean(loss.diff)
    std <- sd(loss.diff) * sqrt(1/P1 + 1/P2)
    kdiff <- ncol(model.matrix(alt(data))) - k.null
    
    if (!is.null(data2)) {
      loss.diff2 <- (L(null.errors2)
                     - L(apply.oos(R2, dfull, alt, window, ret = "error")))
      my <- mean(loss.diff2)
      htest.finish(rval, c(mx, my), c("OOS Avg. 1", "OOS Avg. 2"),
                   (mx-my) / std, "oos-t2", pval((mx-my)/std, k = kdiff),
                   cint(mx, std, k = kdiff), conf.level)
    } else {
      htest.finish(rval, mx, "OOS Avg. 1", mx/std, "oos-t2",
                   pval(mx/std, k = kdiff), cint(mx, std, k = kdiff),
                   conf.level)
    }})
  if (!returnList) tstats <- tstats[[1]]
  tstats
}
