## unit testing --
## pass the function a condition and an error message
## returns TRUE if the condition is true
## returns FALSE and prints the error message if the test is failed

testthis <- function(condition, text, fail = FALSE) {
  if (!isTRUE(condition)) {
    if (fail) stop(text)
    else {
      cat(text, "\n", file = stderr())
      FALSE
    }
  } else {
    TRUE
  }
}

testequal <- function(x1, x2, msg, ...) testthis(all.equal(x1, x2, ...), msg, ...)
