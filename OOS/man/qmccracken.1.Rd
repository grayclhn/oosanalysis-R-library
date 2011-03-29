\name{qmccracken}
\alias{qmccracken.1}
\alias{qmccracken.2}
\alias{pmccracken.1}
\alias{pmccracken.2}
\title{McCracken's (2007) Distribution for oos-t}
\description{}
\usage{
qmccracken.1(p, k, R, P, window = c("fixed", "recursive", "rolling"))

qmccracken.2(p, k, R, P1, P2, window = c("fixed", "recursive", "rolling"))

pmccracken.1(q, k, R, P, window = c("fixed", "recursive", "rolling"), lower.tail = TRUE)

pmccracken.2(q, k, R, P1, P2, window = c("fixed", "recursive", "rolling"), lower.tail = TRUE)
}
\arguments{

  \item{p}{}
  \item{k}{}
  \item{R}{}
  \item{P}{}
  \item{window}{}
  \item{rtRatio}{}
  \item{qtRatio}{}
  \item{q}{}
  \item{lower.tail}{}
}
\details{Add later}
\value{Add later}
\references{M. W. McCracken. Asymptotics for out of sample tests of
  Granger Causality.  Journal of Econometrics, 99(2):195:223, 2000.}
\author{Gray Calhoun}

\seealso{}
\examples{
check <- function(x, y,...) stopifnot(isTRUE(all.equal(x, y,...)))

## compare to the tabluated value of .169
qmccracken.1(.9, 6, 100, 100, "fixed")

## listed value is .234
qmccracken.1(.99, 4, 50, 100, "rolling")

## listed value is 1.193
qmccracken.1(.95, 8, 100, 10, "recursive")

## notice that as P2 grows, the quantiles
## converge to the one-sample quantiles
qmccracken.2(.95, 8, 100, 10, 10, "recursive")
qmccracken.2(.95, 8, 100, 10, 100, "recursive")
qmccracken.2(.95, 8, 100, 10, 1000, "recursive")

## check distribution construction
p <- runif(1)
R <- sample(1:100, 1)
P1 <- sample(1:100, 1)
P2 <- sample(1:100, 1)
k <- sample(1:20, 1)
win <- sample(c("fixed", "rolling", "recursive"), 1)
tol <- 1e-4
check(p, pmccracken.1(qmccracken.1(p, k, R, P1, win),
                      k, R, P1, win, tol = tol), tol = tol)
check(p, pmccracken.2(qmccracken.2(p, k, R, P1, P2, win),
                      k, R, P1, P2, win, tol = tol), tol = tol)
}