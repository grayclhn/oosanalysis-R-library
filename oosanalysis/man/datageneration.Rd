\name{generate.data.mc1}
\alias{generate.data.mc1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generate data for monte carlo}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
generate.data.mc1(n, isPower)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{isPower}{
%%     ~~Describe \code{isPower} here~~
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
function(n, isPower) {
  if (!isPower) {
    gam <- rep(0, n)
  } else if (isPower == 1) {
    gam <- rep(0.35, n)
  } else {
    n1 <- floor(n/2)
    gam <- c(rep(0, n1), rep(.7, n - n1))
  }
  a0 <- .5
  b <- c(.15, .95)
  v <- matrix(c(18, -.5, -.5, .025), 2)
  
  Ez <- b[1] / (1 - b[2])
  Ey <- a0 + gam[1] * Ez

  ## elements of the variance-covariance matrix
  Vz <- v[2,2] / (1 - b[2]^2)
  Vy <- v[1,1] + gam[1]^2 * Vz
  Cyz <- gam[1] * b[2] * Vz + v[1,2]

  ## we're going to let x_t = (y_t, z_t); draw from the stationary
  ## distribution and then populate the rest of the matrix with the
  ## innovations.
  x <- rbind(mvrnorm(1, c(Ey, Ez), matrix(c(Vy, Cyz, Cyz, Vz), 2)),
             mvrnorm(n, c(0,0), v))
  for (i in 1 + (1:n)) {
    A <- matrix(c(0, 0, gam[i-1], b[2]), 2)
    x[i,] = drop(c(a0, b[1]) + A \%*\% x[i-1,] + x[i,])
  }
  data.frame(y = x[-1,1], zlag = x[-n,2])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.