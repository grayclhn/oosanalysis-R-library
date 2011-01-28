qmccracken <- function(kextra, PRratio) {
  if (any(kextra > 10 || kextra < 1))
    stop("kextra must be between 1 and 10")
  if (any(PRratio > 2 || PRratio < 0))
    stop("PRratio must be between 0.0 and 2.0")
  
  ## Recycle the smaller vector per R's conventions.
  kextra.length <- length(kextra)
  PRratio.length <- length(PRratio)
  if (kextra.length < PRratio.length) {
    kextra <- rep(kextra, length.out = PRratio.length)
    ## I want to make sure that kextra.length is correct, since I'm
    ## going to use it later to get the number of elements to return
    ## from the C function call.  I don't care if PRratio.length has
    ## the correct length.
    kextra.length <- PRratio.length
  }
  else if (PRratio.length < kextra.length)
    PRratio <- rep(PRratio, length.out = kextra.length)
  
  .C("qmccracken", 0L, as.integer(kextra), as.double(PRratio),
     as.integer(kextra.length), q = double(kextra.length))$q
}
