.onLoad <- function(lib, pkg) {
  # create new environment
  x12GUIenv <<- new.env()
  ## load data
  # start GUI
  cat("\n X12 GUI is ready to use. Type \"X12GUI()\" to start! \n")}
.onUnload <- function(lib, pkg) {
  rm(x12GUIenv)  
}
.onAttach <- function(...){
  data(AirPassengers)
  if(Sys.info()[1]=="Windows"){
    addTclPath(paste(searchpaths()[grep("x12",searchpaths())],"\\tcl",sep=""))
  }else{
    addTclPath(paste(searchpaths()[grep("x12",searchpaths())],"/tcl",sep=""))
  }
  tclRequire("BWidget")
  #  X12GUI()
}