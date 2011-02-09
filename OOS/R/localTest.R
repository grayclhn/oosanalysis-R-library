## Calculate the wald-test statistic for Clark and McCracken's (2009)
## null hypothesis
##
## y: A vector; the dependent variable in the regression.
##
## x: A matrix; the independent variables of the regression.
##
## x2.index: a logical or numeric vector that indicates the additional
## variables in the larger model; ie x[,x2.index] should be a matrix
## containing the variables we want to test.
##
## method: how should we calculate standard errors?  Must be
## 'homoskedastic' or 'heteroskedastic'
localTest <- function(y, x, x2.index, method = c("homoskedastic", "heteroskedastic")) {
  if (!is.vector(y)) y <- as.vector(y)
  if (!is.matrix(x)) x <- as.matrix(x)
  if (!is.vector(x2.index)) x2.index <- as.vector(x2.index)

  method <- match.arg(method)
  
  n       <- length(y)
  k       <- ncol(x)
  if (is.logical(x2.index)) {
    x1.index <- !x2.index
    k2 <- sum(x2.index)
  } else {
    x1.index <- -x2.index
    k2 <- length(x2.index)
  }
  
  ols     <- lm.fit(y = y, x)
  b2.hat  <- coef(ols)[x2.index]
  Sigma.x <- crossprod(x) / n
  F2.inv  <- (Sigma.x[x2.index, x2.index] -
              crossprod(Sigma.x[x1.index, x2.index, drop = FALSE],
                        solve(Sigma.x[x1.index, x1.index, drop = FALSE],
                              Sigma.x[x1.index, x2.index, drop = FALSE])))
  
  drop(crossprod(b2.hat, crossprod(F2.inv, b2.hat)) -
       switch(method,
              homoskedastic = {
                sigma2.hat <- crossprod(resid(ols)) / (n - k)
                k2 * sigma2.hat
              },
              heteroskedastic = {
                B1 <- matrix(0, k, k)
                B1[-x2.index, -x2.index] <- solve(Sigma.x[-x2.index, -x2.index])
                B2 <- solve(Sigma.x)
                V <- crossprod(crossprod(diag(resid(ols)), x)) / n
                tr(tcrossprod(B2 - B1, V))
              }))
}
