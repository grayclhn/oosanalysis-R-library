 .First.lib <- function(lib, pkg) {
   library.dynam("fwPackage", pkg, lib)
 }

.Last.lib <- function(lib) {
  library.dynam.unload("fwPackage", lib)
}
