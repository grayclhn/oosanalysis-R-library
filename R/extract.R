## Copyright (c) 2011-2013 by Gray Calhoun
 
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or (at
## your option) any later version.

## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
## General Public License for more details.

## For a copy of the GNU General Public License, please see
## <http://www.r-project.org/Licenses/>.

extract_predictors <- function(model, dataset)
  model.matrix(terms(model(dataset[1,])), data = dataset)

extract_target <- function(model, dataset) {
  target <- model.response(model.frame(terms(model(dataset[1,])), dataset))
  if (is.ts(dataset))
    target <- ts(target, start = start(dataset), frequency = frequency(dataset))
  target
}
