defmacro <- function(..., expr){
  # Function by John Fox from the package Rcmdr
  expr <- substitute(expr)
  len <- length(expr)
  expr[3:(len+1)] <- expr[2:len]
  ## delete "macro" variables starting in ..
  expr[[2]] <- quote(on.exit(remove(list=objects(pattern="^\\.\\.", all.names=FALSE))))
  a <- substitute(list(...))[-1]
  ## process the argument list
  nn <- names(a)
  if (is.null(nn)) nn <- rep("", length(a))
  for (i in seq(length=length(a))){
    if (nn[i] == "") {
      nn[i] <- paste(a[[i]])
      msg <- paste(a[[i]], "not supplied")
      a[[i]] <- substitute(stop(foo), list(foo = msg))
    }
  }
  names(a) <- nn
  a <- as.list(a)
  ff <- eval(substitute(
          function(){
            tmp <- substitute(body)
            eval(tmp, parent.frame())
          },
          list(body = expr)))
  ## add the argument list
  formals(ff) <- a
  ## create a fake source attribute
  mm <- match.call()
  mm$expr <- NULL
  mm[[1]] <- as.name("macro")
  expr[[2]] <- NULL # get "local" variable removal out of source
  attr(ff, "source") <- c(deparse(mm), deparse(expr))
  ## return the macro
  ff
}