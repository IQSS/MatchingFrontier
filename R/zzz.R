#Needed to pass CRAN checks for NSE calls with ggplot2
utils::globalVariables(c("Val", "Covariate", "Est", "Var", "Treatment"))

#Used to load backports functions. No need to touch, but must always be included somewhere.
.onLoad <- function(libname, pkgname) {
  backports::import(pkgname)
}

.onAttach <- function(libname, pkgname) {
  version <- packageVersion(pkgname)
  BuildDate <- packageDate(pkgname)

  foo <- paste0(" ", pkgname, " (Version ", version, ", Build Date: ", if (!anyNA(BuildDate)) format(BuildDate, "%F"), ")")
  packageStartupMessage(foo)
}
