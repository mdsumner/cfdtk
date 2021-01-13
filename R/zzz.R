.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Climate Futures Development Toolkit")
  packageStartupMessage("Create INFO logger and attach console handler")
  logging::basicConfig()
  packageStartupMessage()
}
