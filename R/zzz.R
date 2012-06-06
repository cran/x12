.onLoad <- function(lib, pkg) {
  packageStartupMessage("x12 is ready to use.")
  packageStartupMessage("Load the package x12GUI for a Graphical User Interface. \n")
  packageStartupMessage("It is advised to create the variables \"x12path\" and \"x13path\"")
  packageStartupMessage("with valid paths to the X12 or X13 executables.\n")
}
.onAttach <- function(...){
  data(AirPassengers)
}