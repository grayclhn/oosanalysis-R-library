## Copyright (C) 2011-2015 Gray Calhoun; MIT license

extract_predictors <- function(model, dataset)
  model.matrix(terms(model(dataset[1,])), data = dataset)

extract_target <- function(model, dataset) {
  target <- model.response(model.frame(terms(model(dataset[1,])), dataset))
  if (is.ts(dataset))
    target <- ts(target, start = start(dataset), frequency = frequency(dataset))
  target
}
