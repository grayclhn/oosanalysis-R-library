library(dbframe)

mcdata <- dbframe("mc", "mccracken.db")
index(mcdata) <- c("t1", "t2", "k", "window")
