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

cint(method = c("DMW", "Mcc:07"),
     alternative = c("greater", "less", "two.sided"),
     window = c("fixed", "recursive", "rolling"),
     avg, std, k, P1, P2, R, conf.level)

pval(method = c("DMW", "Mcc:07"),
     alternative = c("greater", "less", "two.sided"),
     window = c("fixed", "recursive", "rolling"),
     tstat, k, P1, P2, R, conf.level)
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
check <- function(a, b = .Last.value,...)
  stopifnot(isTRUE(all.equal(unname(a), unname(b),
            check.attributes = FALSE,...)))

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
p
R <- sample(1:100, 1)
P1 <- sample(1:100, 1)
P2 <- sample(1:100, 1)
k <- sample(1:20, 1)
win <- sample(c("fixed", "rolling", "recursive"), 1)

tol = 1e-4
pmccracken.1(qmccracken.1(p, k, R, P1, win),
             k, R, P1, win, tol = tol)
check(p, tol = 1e-4)

pmccracken.2(qmccracken.2(p, k, R, P1, P2, win),
             k, R, P1, P2, win, tol = tol)
check(p, tol = 1e-4)

## We're just going to do a bunch of tests for "DMW"
## "DMW" doesn't depend on the window type

sapply(c("rolling", "recursive", "fixed"), function(w)
  pval("DMW", "greater", w, 1.65, 1, 1000, 1000, 1000))
check(rep(pt(-1.65, 999), 3))

sapply(c("rolling", "recursive", "fixed"), function(w)
  pval("DMW", "less", w, 1.65, 1, 1000, 1000, 1000))
check(rep(pt(1.65, 999), 3))

sapply(c("rolling", "recursive", "fixed"), function(w)
  pval("DMW", "two.sided", w, 1.65, 1, 1000, 1000, 1000))
check(rep(2 * pt(-1.65, 999), 3))

sapply(c("rolling", "recursive", "fixed"), function(w)
  cint("DMW", "greater", w, 0, 1, 1, 1000, 1000, 1000, .9))
check(rbind(rep(qt(.1, 999), 3), Inf))

sapply(c("rolling", "recursive", "fixed"), function(w)
  cint("DMW", "less", w, 0, 1, 1, 1000, 1000, 1000, .9))
check(rbind(-Inf, rep(qt(.9, 999), 3)))

sapply(c("rolling", "recursive", "fixed"), function(w)
  cint("DMW", "two.sided", w, 0, 1, 1, 1000, 1000, 1000, .9))
check(qt(rbind(.05, rep(.95, 3)), 999))

sapply(c("rolling", "recursive", "fixed"), function(w)
  cint("DMW", "two.sided", w, 1, 1, 1, 1000, 1000, 1000, .9))
check(1 + qt(rbind(.05, rep(.95, 3)), 999))
}