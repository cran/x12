setGeneric("X12",
    function(object, x12Parameter=new("x12Parameter"),
        x12BaseInfo=new("x12BaseInfo"),...) { standardGeneric("X12")} )
setMethod(
    f='X12',
    signature=signature(object = "ts"),
    definition=function(object, x12Parameter,x12BaseInfo) {
      Par <- slotNames(x12Parameter)
      pp <- vector()
      for(p in Par){
        pp <- c(pp,(paste(p,"=x12Parameter@",p,sep="")))
      }
      Par <- slotNames(x12BaseInfo)
       for(p in Par){
         pp <- c(pp,(paste(p,"=x12BaseInfo@",p,sep="")))
       }
      pp <- paste("out <- x12(tso=object,",paste(pp,collapse=","),")",sep="")
      eval(parse(text=pp))
      classout <- new("x12Output")
      Par <- slotNames(classout)
       for(p in Par){
         if(class(slot(classout,p))=="spectrum"){
           if(p%in%names(out)){
             slot(classout,p)@frequency <- out[[p]]$frequency
             slot(classout,p)@spectrum <- out[[p]]$spectrum
           }
         }else if(class(slot(classout,p))=="fbcast"){
           if(p%in%names(out)){
             slot(classout,p)@estimate <- out[[p]][["estimate"]]
             slot(classout,p)@lowerci <- out[[p]][["lowerci"]]
             slot(classout,p)@upperci <- out[[p]][["upperci"]]
           }
         }else
           slot(classout,p)<-out[[p]] 
       }
      return(classout)
    }
)
setMethod(
    f='X12',
    signature=signature(object = "x12Single"),
    definition=function(object,x12BaseInfo=new("x12BaseInfo"),forceRun=FALSE) {
      if(length(object@x12OldParameter)>0)
        TF <- !identical(object@x12Parameter,object@x12OldParameter[[length(object@x12OldParameter)]])
      else
        TF <- TRUE
      if(!object@firstRun||forceRun||TF){
        x12Parameter <- object@x12Parameter  
        if(object@firstRun){
          object@x12OldParameter[[length(object@x12OldParameter)+1]] <- object@x12Parameter
          object@x12OldOutput[[length(object@x12OldOutput)+1]] <- object@x12Output
        }
        object@firstRun <- TRUE
        Par <- slotNames(x12Parameter)
        pp <- vector()
        for(p in Par){
          pp <- c(pp,(paste(p,"=x12Parameter@",p,sep="")))
        }
        Par <- slotNames(x12BaseInfo)
        for(p in Par){
          pp <- c(pp,(paste(p,"=x12BaseInfo@",p,sep="")))
        }
        
        if(!is.null(object@tsName))
          pp <- c(pp, paste("file=\"",object@tsName,"\"",sep=""))
        pp <- paste("out <- x12(tso=object@ts,",paste(pp,collapse=","),")",sep="")
        eval(parse(text=pp))
        classout <- new("x12Output")
        Par <- slotNames(classout)
        for(p in Par){
          if(class(slot(classout,p))=="spectrum"){
            if(p%in%names(out)){
              slot(classout,p)@frequency <- out[[p]]$frequency
              slot(classout,p)@spectrum <- out[[p]]$spectrum
            }
          }else if(class(slot(classout,p))=="fbcast"){
            if(p%in%names(out)){
              slot(classout,p)@estimate <- out[[p]][["estimate"]]
              slot(classout,p)@lowerci <- out[[p]][["lowerci"]]
              slot(classout,p)@upperci <- out[[p]][["upperci"]]
            }
          }else
            slot(classout,p)<-out[[p]] 
        }
        object@x12Output <- classout
        
      }
      return(object)
    }
)
setMethod(
    f='X12',
    signature=signature(object = "x12Batch"),
    definition=function(object,forceRun=FALSE) {
      if(any(ls(1)=="x12path"))
        object@x12BaseInfo@x12path <- get("x12path",as.environment(1))
      else
        stop("Please enter a x12path")
      starting.time <- Sys.time()
      for(i in 1:length(object@x12List)){
        object@x12List[[i]] <- X12(object@x12List[[i]],x12BaseInfo=object@x12BaseInfo,forceRun=forceRun)
      }
      print(Sys.time()-starting.time)
      return(object)
   }
)
