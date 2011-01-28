`neweyWest` <-
function(Z, nlags = ceiling(nobs^.25)) {
  
### Returns the Newey-West estimator of the asymptotic variance matrix
### INPUTS: - Z, a nxk matrix with rows the vector zt'
###         - nlags, the number of lags
###
### OUTPUTS: - avarEstimate, the Newey-West estimator of the long-run 
###            covariance matrix

  if (!is.matrix(Z)) {
    dim(Z) <- c(length(Z), 1)
  }
  
  nobs <- dim(Z)[1]
  meanZ <- colMeans(Z)
    
  productMeanZ <- meanZ %o% meanZ
  avarEstimate <- crossprod(Z)/nobs - productMeanZ

  if (nlags > 0 ) {
    currentSum <- nobs * meanZ
    laggedSum  <- nobs * meanZ
  
    for (lag.i in 1:nlags) {
      index.i <- (1+lag.i):nobs
      
      ## The original code demeaned Z and then calculated gamma_i as:
      ## Z(index_i,:)' * Z(index_i - lag.i,:).
      ##
      ## I don't want to copy Z into local memory (R will pass a pointer to
      ## the functions arguments _unless_ you modify the variable inside a
      ## function.  In that case, it copies the variable into memory.), so I
      ## subtract the mean and expand the product explicitly.  Expanding the
      ## product lets us work with nSeries x nSeries matrices instead of nobs x
      ## nSeries matrices.
      
      ## sum of the elements in Z[index_i,]
      currentSum <- currentSum - Z[lag.i,]
      ## sum of the elements in Z[index_i - lag.i,]
      laggedSum <- laggedSum - Z[nobs - lag.i + 1,]
      
      gamma.i <- crossprod(Z[index.i,], Z[index.i - lag.i,]) -
        meanZ %o% laggedSum - currentSum %o% meanZ + (nobs - lag.i) * productMeanZ;
      
      ## weight (gamma.i + gamma.i') / nobs according to the Bartlett kernel and 
      ## add to the long-run covariance estimate.
      avarEstimate <- avarEstimate +
        (1 - lag.i/(nlags+1)) * (gamma.i + t(gamma.i))/nobs;
    }
  }
  avarEstimate
}

