## Copyright (C) 2011-2015 Gray Calhoun; MIT license

recursive_forecasts <- function(model, dataset, R,
                         window = c("recursive", "rolling", "fixed"),...) {
  window <- match.arg(window)
  n <- nrow(dataset)
  if (R >= n) stop("dataset must have more than R observations")
  getprediction <- function(firstobs, lastobs, horizon)
    predict(model(dataset[seq(firstobs, lastobs, by = 1),],...),
            newdata = dataset[lastobs + horizon,])
  switch(window,
         recursive = sapply((R+1):n, function(s) getprediction(1, s-1, 1)),
         rolling   = sapply((R+1):n, function(s) getprediction(s-R, s-1, 1)),
         fixed     = getprediction(1, R, 1:(n-R)))
}                 
