EL <- function(b.hat, b) {
  b.error <- b.hat - b
  drop(1 + crossprod(b.error))
}
