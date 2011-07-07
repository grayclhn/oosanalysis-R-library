library(dbframe)

mcdata <- dbframe("mc", "mccracken.db")
qdata <- dbframe("qmc1", "mccracken.db")
clear(qdata)

tvals <- expand.grid(t1 = seq(2L, 98L, 2L), t2 = seq(2L, 98L, 2L))
tvals <- tvals[tvals$t1 < tvals$t2,]
row.names(tvals) <- NULL

maxk <- 15

fullvals <- expand.grid(window = c("recursive", "fixed", "rolling"), k = 1:maxk,
                        t1 = unique(tvals$t1), t2 = unique(tvals$t2))

fullvals <- fullvals[fullvals$t1 < fullvals$t2,]
row.names(fullvals) <- NULL

for (s in seq_len(nrow(fullvals))) {
  d <- select(mcdata, where = sprintf("t1=%d and t2=%d and k=%d and window='%s'",
                        fullvals[s,"t1"], fullvals[s,"t2"], fullvals[s,"k"], fullvals[s,"window"]))
  d$M1a <- with(d, (G1 - G2/2) / sqrt(G2))
  d$M1b <- with(d, (L1 - L2/2) / sqrt(L2))
  d$M2 <-  with(d, (G1 - G2/2 + sqrt((t2-t1)/(100-t2)) * (L1 - L2/2)) / sqrt(G2))

  insert(qdata) <-
    dfapply(c(75L, 90L, 95L, 99L), function(quant) {
      data.frame(fullvals[s,], quant = quant,
                 lapply(subset(d,,c(M1a, M1b, M2)),
                        function(x) quantile(x, quant/100)))
    })
}
