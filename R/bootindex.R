## Copyright (C) 2011-2015 Gray Calhoun; MIT license

bootindex_movingblock <- function(nobs, blocklength) {
  blockstarts <- sample(seq_len(nobs - blocklength + 1),
                        ceiling(nobs / blocklength),
                        replace = TRUE)
  indices <- 
    as.vector(sapply(blockstarts, function(s) s:(s + blocklength - 1)))[1:nobs]
  attr(indices, "starts") <- blockstarts
  attr(indices, "lengths") <- blocklength
  indices
}

bootindex_circularblock <- function(nobs, blocklength) {
  blockstarts <- sample(seq_len(nobs), ceiling(nobs / blocklength), 
                        replace = TRUE)
  indices <- 
    as.vector(sapply(blockstarts, function(s)
                     (s + seq_len(blocklength) - 1) %% nobs + 1))[seq_len(nobs)]
  attr(indices, "starts") <- blockstarts
  attr(indices, "lengths") <- blocklength
  indices
}

bootindex_stationary <- function(nobs, blocklength) {
  blockstarts <- sample(seq_len(nobs), replace = TRUE) 
  blocklengths <- rpois(nobs, blocklength)
  while(sum(blocklengths) < nobs) {
    blockstarts <- c(blockstarts, sample(seq_len(nobs), replace = TRUE))
    blocklengths <- c(blocklengths, rpois(nobs, blocklength))
  }
  fullblocks <- sum(cumsum(blocklengths) < nobs)
  blockstarts <- blockstarts[seq_len(fullblocks + 1)]
  blocklengths <- c(blocklengths[seq_len(fullblocks)],
                    nobs - sum(blocklengths[seq_len(fullblocks)]))
  indices <- unlist(sapply(seq_len(fullblocks + 1), function(s)
                           1 + seq.int(from = blockstarts[s] - 1, 
                                       length = blocklengths[s]) %% nobs))
  
  attr(indices, "starts") <- blockstarts[blocklengths != 0]
  attr(indices, "lengths") <- blocklengths[blocklengths != 0]
  indices
}
