\name{MakeFixedForecastPair}
\alias{MakeFixedForecastPair}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Estimate out-of-sample forecasts and save as a FixedForecastPair.
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MakeFixedForecastPair(X, Y, ntrain, nreg1, nreg2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{ntrain}{
%%     ~~Describe \code{ntrain} here~~
}
  \item{nreg1}{
%%     ~~Describe \code{nreg1} here~~
}
  \item{nreg2}{
%%     ~~Describe \code{nreg2} here~~
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
compare <- function(a, b)
  stopifnot(all.equal(unname(a), unname(b)))

X <- matrix(rnorm(200), 50, 4)
Y <- rnorm(50)
d <- data.frame(Y, X)

mod1  <- lm(Y ~ 1, d, subset = 1:40)
mod2  <- lm(Y ~ X1 + X2 + X3 + X4, d, subset = 1:40)

pair <- MakeFixedForecastPair(cbind(1,X), Y, 40, 1, 5)

compare(target(pair), Y[41:50])
compare(coef(mod1), coef(pair)[[1]])
compare(coef(mod2), coef(pair)[[2]])

compare(vcov(mod1), as.matrix(vcov(pair)[[1]]))
compare(vcov(mod2), as.matrix(vcov(pair)[[2]]))

compare(predict(mod1, newdata = d[41:50,]),
        predict(pair)[, 1, drop = TRUE])
compare(predict(mod2, newdata = d[41:50,]),
        predict(pair)[, 2, drop = TRUE])
}