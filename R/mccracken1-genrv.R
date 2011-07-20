library(dbframe)

rBM <- function(k, ngrain) {
  increments <- matrix(rnorm(ngrain * k, 0, 1/sqrt(ngrain)), k, ngrain)
  apply(increments, 1, cumsum)
}

recterm1 <- function(tindex, B, dB, sinv)
  sum(sinv[tindex] * B[tindex,] * dB[tindex,])

recterm2 <- function(tindex, B, sinv, ds)
  sum(sinv[tindex]^2 * B[tindex,]^2) * ds

Blag <- function(tindex, shift, B) {
  lindex <- tindex - shift
  stopifnot(all(lindex >= 0))
  if (lindex[1] == 0) {
    rbind(0, B[lindex[-1],,drop=FALSE])
  } else {
    B[lindex,,drop=FALSE]
  }
}

rollterm1 <- function(tindex, shift, B, dB, lend)
  sum((B[tindex,] - Blag(tindex, shift, B)) * dB[tindex,]) / lend

rollterm2 <- function(tindex, shift, B, ds, lend)
  sum((B[tindex,] - Blag(tindex, shift, B))^2) * ds / lend^2

fixterm1 <- function(tindex, B, lend) {
  n <- length(tindex)
  B1 <- B[tindex[1],]
  drop(crossprod(B[tindex[n],] - B1, B1)) / lend
}

fixterm2 <- function(tindex, B, p, lend)
  p * drop(crossprod(B[tindex[1],])) / lend

mccrackenvars <- function(k, train1, train2, ngrain = 10000) {
  stopifnot(all(train1 > 0) && all(train2 > train1) && all(train2 < 1))
  
  B <- rBM(k, ngrain)
  dB <- diff(B)
  B <- B[-ngrain,,drop=FALSE]
  n <- ngrain - 1
  sinv <- 1/seq(from = 0, to = 1 - 1/n, length = n)

  mapply(function(t1, t2) {
    test1 <- ceiling(t1 * ngrain):ceiling(t2 * ngrain - 1)
    test2 <- ceiling(t2 * ngrain):n
    shift <- test1[1]
    c(recG1 = recterm1(test1, B, dB, sinv),
      recL1 = recterm1(test2, B, dB, sinv),
      recG2 = recterm2(test1, B, sinv, 1/ngrain),
      recL2 = recterm2(test2, B, sinv, 1/ngrain),
      fixG1 = fixterm1(test1, B, t1),
      fixL1 = fixterm1(test2, B, t2),
      fixG2 = fixterm2(test1, B, (t2 - t1) / t1, t1),
      fixL2 = fixterm2(test2, B, (1 - t2) / t2, t2),
      rollG1 = rollterm1(test1, shift, B, dB, t1),
      rollL1 = rollterm1(test2, shift, B, dB, t1),
      rollG2 = rollterm2(test1, shift, B, 1/ngrain, t1),
      rollL2 = rollterm2(test2, shift, B, 1/ngrain, t1))
  }, t1 = train1, t2 = train2)
}

mccrackent1 <- function(G1, G2)
  quantile((G1 - G2/2) / sqrt(G2), c(.9, .95, .99))
mccrackent2 <- function(G1, G2, L1, L2, testratio)
  (G1 - G2/2 - sqrt(testratio) * (L1 - L2/2)) / sqrt(G2)

mcdata <- dbframe("mc", "mccracken.db")
clear(mcdata)

tvals <- expand.grid(t1 = seq(2L, 98L, 2L), t2 = seq(2L, 98L, 2L))
tvals <- tvals[tvals$t1 < tvals$t2,]
row.names(tvals) <- NULL
nsim <- 5000
maxk <- 15

for (i in 1:nsim) {
  cat(i,"/", nsim,". . .")
  insert(mcdata) <-
    dfapply(1:maxk, function(k) {
      rvs <- with(tvals, mccrackenvars(k, t1/100, t2/100))
      data.frame(sim = i, k = k, tvals,
                 rbind(data.frame(window = "recursive", G1 = rvs[ 1,],
                                  L1 = rvs[ 2,], G2 = rvs[ 3,], L2 = rvs[ 4,]),
                       data.frame(window = "fixed", G1 = rvs[ 5,],
                                  L1 = rvs[ 6,], G2 = rvs[ 7,], L2 = rvs[ 8,]),
                       data.frame(window = "rolling", G1 = rvs[ 9,],
                                  L1 = rvs[10,], G2 = rvs[11,], L2 = rvs[12,])))
    })
}
