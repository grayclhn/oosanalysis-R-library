Ftest <- function(X.null, X.alt, Y) {
  nreg.alt <- ncol(X.alt)
  
  e.null <- lm.fit(X.null, Y)$residuals
  e.alt  <- lm.fit(X.alt, Y)$residuals
  crossprod.alt <- crossprod(e.alt)
  drop(((crossprod(e.null) - crossprod.alt) / (nreg.alt - ncol(X.null)))
   / (crossprod.alt / (length(e.alt) - nreg.alt)))
}
