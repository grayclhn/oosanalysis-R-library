\name{extract.target}
\alias{extract.target}
\alias{extract.predictors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convenience function to extract data from a model}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
extract.target(model, dataset)
extract.predictors(model, dataset)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{dataset}{
%%     ~~Describe \code{dataset} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(model, dataset) {
  target <- model.response(model.frame(terms(model(dataset[1,])), dataset))
  if (is.ts(dataset))
    target <- ts(target, start = start(dataset), frequency = frequency(dataset))
  target
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
