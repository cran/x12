### Class definitions ###
## Types for representations
setClassUnion("listOrNULL", c("list", "NULL"))
setClassUnion("tsOrNULL", c("ts", "NULL"))
setClassUnion("matrixOrNULL", c("matrix", "NULL"))
setClassUnion("dfOrNULL", c("data.frame", "NULL"))
setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("listOrNULLOrnumeric", c("list", "numeric","NULL"))
setClassUnion("listOrNULLOrcharacter", c("list", "character","NULL"))
### Parameter Object class: x12Parameter ###
setClass(
    Class="x12Parameter", 
    representation=representation(			
      #period="numeric",
      span="numericOrNULL",
      modelspan="numericOrNULL",
      decimals="numeric",
      transform="character",
		  arima="numericOrNULL",
      sarima="numericOrNULL",
			automdl="logical",
      acceptdefault="logical",
      balanced="logical",
      maxorder="numeric",
      maxdiff="numeric",
      regvariables="characterOrNULL",
      reguser="characterOrNULL",
      regfile="characterOrNULL",
      usertype="characterOrNULL",
      centeruser="characterOrNULL",
      regfilestart="characterOrNULL",
      #regfileformat="characterOrNULL",
      #tblnames="characterOrNULL",
      #Rtblnames="characterOrNULL",
      #addLines="listOrNULL",
      #use="character",
			seats="logical",
      seatsparameter="characterOrNULL",
      sigmalim="numericOrNULL",
      outlier="characterOrNULL",
      critical="listOrNULLOrnumeric",
      outlier_span="numericOrNULL",
      outlier_method="characterOrNULL",
			forecast_years="numericOrNULL",
      backcast_years="numericOrNULL",
      forecast_conf="numeric",
      estimate="logical",
      estOutofsample="logical",
 	  	slidingspans="logical",
      aictest="characterOrNULL",
      onlytd="logical",
      sfshort="logical",
      samode="characterOrNULL",
      seasonalma="characterOrNULL",
			trendma="numericOrNULL",
      x11appendfcst = "logical",
      x11appendbcst = "logical",
      x11calendarsigma = "characterOrNULL",
      x11excludefcst = "logical", 
      x11final = "character",
      x11regress = "logical"
    ),
    prototype=prototype(
      #period = 12,
      span = NULL,
      modelspan = NULL, 
      decimals = 2,
      transform = "auto",
      arima = NULL,
      sarima = NULL,
      automdl = TRUE,
      acceptdefault=FALSE,
      balanced=TRUE,
      maxorder = c(3, 2),
      maxdiff = c(1, 1),
      regvariables = NULL,
      reguser = NULL,
      regfile = NULL,
      usertype = NULL,
      centeruser = NULL,
      regfilestart = NULL,
      #tblnames = NULL,
      #Rtblnames = NULL,
      #addLines = NULL,
      #use = "x12",
      seats = FALSE,
      seatsparameter = NULL,
      sigmalim=NULL,
      critical = NULL,
      outlier_span = NULL,
      outlier_method = NULL,
      forecast_years = 1,
      backcast_years = NULL,
      forecast_conf=0.95,
      estimate = FALSE,
      estOutofsample = TRUE,
      slidingspans = FALSE,
      aictest = NULL,
      onlytd = FALSE,
      sfshort = FALSE, 
      samode = NULL,
      seasonalma = NULL,
      trendma = NULL,
      x11appendfcst = TRUE, 
      x11appendbcst = FALSE,
      x11calendarsigma = NULL,
      x11excludefcst = TRUE, 
      x11final = "user", x11regress = FALSE
    ),
    validity=function(object) {
      return(TRUE)
    }
)
setClass(
    Class="spectrum", 
    representation=representation(
      frequency="numeric",
      spectrum="numeric"
    ),prototype=
        prototype(
        frequency=new("numeric"),
        spectrum=new("numeric")
    ),
    validity=function(object) {
      length(object@spectrum)==length(object@frequency)
    }
)
setClass(
    Class="fbcast",
    representation=representation(
        estimate="ts",
        lowerci="ts",
        upperci="ts"
    ),prototype=
        prototype(
            estimate=new("ts"),
            lowerci=new("ts"),
            upperci=new("ts")
        ),
    validity=function(object) {
      length(object@estimate)==length(object@lowerci)&&length(object@estimate)==length(object@upperci)
    }
)
setClass(
    Class="x12BaseInfo",
    representation=representation(
            x12path = "characterOrNULL",
            x13path = "characterOrNULL",
            use = "character",
            showWarnings = "logical" 
    ),prototype=
        prototype(
            x12path = NULL,
            x13path = NULL,
            use = "x12",
            showWarnings = FALSE
        ),
    validity=function(object) {
      (!is.null(object$x12path)||!is.null(object$x13path))&&object$use%in%c("x12","x13")
    }
)
setClass(Class="diagnostics",contains="list")
### Output Object class: x12Output ###
setClass(
    Class="x12Output", 
    representation=representation(			
      a1="ts",
      d10="ts",
      d11="ts",
      d12="ts",
      d13="ts",
      d16="ts",
      c17="ts",
      d9="ts",
      e2="ts",
      d8="ts",
      b1="ts",
      otl="tsOrNULL",
      sp0="spectrum",
      sp1="spectrum",
      sp2="spectrum",
      spr="spectrum",
      forecast="fbcast",
      backcast="fbcast",
      dg="list",
      seats="logical",
      file="character",
      tblnames="character",
      Rtblnames="character"
    ),
    prototype=prototype(
        a1=new("ts"),
        d10=new("ts"),
        d11=new("ts"),
        d12=new("ts"),
        d13=new("ts"),
        d16=new("ts"),
        c17=new("ts"),
        d9=new("ts"),
        e2=new("ts"),
        d8=new("ts"),
        b1=new("ts"),
        sp0=new("spectrum"),
        sp1=new("spectrum"),
        sp2=new("spectrum"),
        spr=new("spectrum"),
        forecast=new("fbcast"),
        backcast=new("fbcast"),
        dg=new("diagnostics"),
        seats=new("logical"),
        file=new("character"),
        tblnames=new("character"),
        Rtblnames=new("character")        
    ),
    validity=function(object) {
      return(TRUE)
    }
)

setClass(
    Class="x12Single", 
    representation=representation(			
        ts="ts",
        x12Parameter="x12Parameter",
        x12Output="x12Output",
        x12OldParameter="list",
        x12OldOutput="list",
        
        tsName="characterOrNULL",
        firstRun="logical"
    ),
    prototype=prototype(
        ts=new("ts"),
        x12Parameter=new("x12Parameter"),
        x12Output=new("x12Output"),
        x12OldParameter=new("list"),
        x12OldOutput=new("list"),
        tsName=NULL,
        firstRun=FALSE
),
    validity=function(object) {
      return(TRUE)
    }
)
setClass(Class="x12List",contains="list",validity=function(object){
      all(lapply(object,class),"x12Single")      
    })
setClass(
    Class="x12Batch", 
    representation=representation(			
        x12List="x12List",
        x12BaseInfo="x12BaseInfo"
    ),
    prototype=prototype(
        x12List=new("x12List"),
        x12BaseInfo=new("x12BaseInfo")
    ),
    validity=function(object) {
      return(TRUE)
    }
)

setMethod(
    f='initialize',
    signature=signature(.Object = "x12Batch"),
    definition=function(.Object,tsList,tsName=NULL,x12BaseInfo=new("x12BaseInfo")) {
      for(i in 1:length(tsList)){
        if(class(tsList[[i]])=="x12Single")
          .Object@x12List[[i]] <- tsList[[i]]
        else{
          if(!is.null(tsName))
            .Object@x12List[[i]] <-new("x12Single",ts=tsList[[i]],tsName=tsName[i])
          else{
            .Object@x12List[[i]] <-new("x12Single",ts=tsList[[i]],tsName=paste("Series_",i,sep=""))
          }
        }
      }
      .Object@x12BaseInfo <- x12BaseInfo
      return(.Object)
    }
)
###Handling of x12path
setMethod(
    f='initialize',
    signature=signature(.Object = "x12BaseInfo"),
    definition=function(.Object,x12path=NULL,x13path=NULL,use="x12",showWarnings=FALSE) {
      if(is.null(x12path)&&is.null(x13path)&&!exists("x12path",1)&&!exists("x13path",1))
        stop("You have to give either x12path or x13path as argument or \n
              specify a variable x12path in the global environment")
     if(is.null(x12path)&&exists("x12path",1)){
       if(file.exists(get("x12path",1)))
         .Object@x12path <- get("x12path",1)
       else
         stop("file specified in global variable x12path does not exist!\n")
    }
    if(is.null(x13path)&&exists("x13path",1)){
      if(file.exists(get("x13path",1)))
        .Object@x13path <- get("x13path",1)
      else
        stop("file specified in global variable x13path does not exist!\n")
    }
     if(!is.null(x12path)){
       if(file.exists(x12path))
         .Object@x12path <- x12path
       else
         stop("file specified in argument x12path does not exist!\n")
     }
     if(!is.null(x13path)){
       if(file.exists(x13path))
         .Object@x13path <- x13path
       else
         stop("file specified in argument x13path does not exist!\n")
     }
     if(use=="x12"&&is.null(.Object@x12path))
       stop("use=x12 but no x12path specified")
     if(use=="x13"&&is.null(.Object@x13path))
       stop("use=x13 but no x13path specified")
     if(!use%in%c("x12","x13"))
       stop("argument use must be either \"x12\" or \"x13\" !\n ")
     .Object@use <- use
     .Object@showWarnings <- showWarnings
      return(.Object)
    }
)
#Basic methods for x12Batch and x12Single
setMethod(
    f='dim',
    signature=signature(x = "x12Batch"),
    definition=function(x) {
          return(length(x@x12List))
    }
)
setMethod(
    f='length',
    signature=signature(x = "x12Batch"),
    definition=function(x) {
      return(length(x@x12List))
    }
)

setMethod(
    f='print',
    signature=signature(x = "x12Batch"),
    definition=function(x) {
      cat("A batch of time series of length ",length(x@x12List),".\n")
      for(i in 1:length(x@x12List))
        print(x@x12List[[i]])
    }
)

setMethod(
    f='print',
    signature=signature(x = "x12Single"),
    definition=function(x) {
      cat("Name: ",x@tsName,"\n")
      cat("processed with X12: ",x@firstRun,"\n")
      print(x@ts)
    }
)



setClass(Class="crossValidation",
		representation=representation(
				backcast="dfOrNULL",
				forecast="dfOrNULL"),
		prototype=prototype(
				backcast=NULL,
				forecast=NULL),
#		validity=function(object) {
#			length(object@spectrum)==length(object@frequency)
#		}
)
