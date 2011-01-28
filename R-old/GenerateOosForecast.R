GenerateOosForecast <- function(X.full, Y, ntrain, nreg, b.true) {
  nreg.full <- length(b.true)
  nreg.excess <- nreg.full - nreg
  stopifnot(nreg.full == ncol(X.full))

  X <- X.full[, 1:nreg, drop = FALSE]

  PaddedEstimates <- function(coef) c(coef, rep.int(0, nreg.excess))
  
  ntrain <- sort.int(ntrain)
  pred.list <- lapply(ntrain, function(R) {
    coef <- lm.fit(X[1:R, ], Y[1:R])$coefficients
    list(coef = PaddedEstimates(coef),
         pred = drop(tcrossprod(coef, X[-(1:R), ])))
  })
  new("oos.forecast",
      expected.loss.test = sapply(pred.list, function(p) EL(p$coef, b.true)),
      expected.loss.future = EL(PaddedEstimates(lm.fit(X, Y)$coefficients), b.true),
      forecasts = lapply(pred.list, function(p) p$pred),
      target = Y[-(1:ntrain[1])],
      nreg = as.integer(nreg))
}
