oldRMccrackenFixed <- function(PRratio, k, nsims, ngrain) {

  gam1 <- gam2 <- matrix(NA, length(PRratio), nsims)
  for (i in 1:length(PRratio)) {
    rolling <- .C("gamRolling",
                  as.integer(nsims),
                  as.integer(ngrain),
                  as.integer(k),
                  as.double(PRratio[i]),
                  gam1 = as.double(rep(0, nsims)),
                  gam2 = as.double(rep(0, nsims)))
    gam1[i,] <- rolling$gam1
    gam2[i,] <- rolling$gam2
  }
  list(gam1 = gam1, gam2 = gam2)
}

swMcCrackenExpanding <- function(r, nobs, k=1, ngrain, n1 = 1) {
  nt <- ngrain
  nrep <- nsims

  s <- nt / 1:nt;  
  ii <- floor(nt*((r+1)/nobs));

  dw.SW <- matrix(rnorm(nt * n1) / sqrt(nt), nt, n1);
  w.SW <- apply(dw.SW, 2, cumsum);

  sw <- s[(ii-1):(nt-1)] * w.SW[(ii-1):(nt-1),,drop=FALSE];

  winte <- colSums(sw * dw.SW[ii:nt,,drop = FALSE]);
  wintd <- 2 * winte - (colSums(sw*sw)/nt)
  oost <- wintd / (2 * sqrt(colSums(sw*sw)/nt))

  r <- cbind(winte, colSums(sw*sw)/nt)
  colnames(r) <- c("gam1", "gam2")
  r
}
