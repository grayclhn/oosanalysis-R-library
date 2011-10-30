\name{dmw}
\alias{dmw}
\alias{dmw.calculation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Diebold-Mariano-West out-of-sample t-test}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dmw.calculation(f, h, BF, R, vcv, window = c("recursive", "rolling", "fixed"))
dmw.lambda(pi, window = c("recursive", "rolling", "fixed"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{
%%     ~~Describe \code{f} here~~
}
  \item{h}{
%%     ~~Describe \code{h} here~~
}
  \item{BF}{
%%     ~~Describe \code{BF} here~~
}
  \item{R}{
%%     ~~Describe \code{R} here~~
}
  \item{vcv}{
%%     ~~Describe \code{vcv} here~~
}
  \item{window}{
%%     ~~Describe \code{window} here~~
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
function(f, h, BF, R, vcv, 
                            window = c("recursive", "rolling", "fixed")) {
  lambda <- dmw.lambda(pi = length(f)/R, window)
  hBF <- tcrossprod(as.matrix(h), BF)
  S <- vcv(cbind(f, hBF))
  return(list(mu = mean(f), sd = sqrt(S[1,1] + lambda$fh * (S[1,2] + S[2,1]) 
                                      + lambda$hh * S[2,2])))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
