x12 <- function(tso,period=frequency(tso),span=NULL,modelspan=NULL,
				decimals=2,transform="auto",
		        arima=NULL,sarima=NULL,
				automdl=FALSE,acceptdefault=FALSE,balanced=TRUE,maxorder=c(3,2),maxdiff=c(1,1),
				regvariables=NULL,reguser=NULL,regfile=NULL,usertype=NULL,centeruser=NULL,regfilestart=NULL,#regfileformat=NULL,
                tblnames=NULL,Rtblnames=NULL,addLines=NULL,
                x12path=NULL,x13path=NULL,use="x12",
				seats=FALSE, seatsparameter=NULL,
                sigmalim=c(1.5,2.5),outlier=NULL,critical=NULL,outlier_span=NULL,outlier_method=NULL,
				file="Rout",forecast_years=NULL,backcast_years=NULL,forecast_conf=.95,estimate=FALSE,
				estOutofsample=TRUE,slidingspans=FALSE,aictest=NULL,
				onlytd=FALSE,sfshort=FALSE,samode=NULL,seasonalma=NULL,trendma=NULL,
				x11appendfcst=TRUE,x11appendbcst=FALSE,x11calendarsigma=NULL,x11excludefcst=TRUE,x11final="user",
				x11regress=FALSE,keep_x12out=FALSE,showWarnings=FALSE
){
#addLines=list(list("series","bla=blabla"),list("regression","bla2=soundso","bla4=diesunddas"))
file.remove(grep(basename(file),list.files(dirname(file)),value=TRUE))
unlink(paste(dirname(file),"/gra",sep=""),recursive=TRUE)
if((length(tso)/period)>15 && !is.null(backcast_years) && !showWarnings){
cat("\nWarning: x12 cannot produce backcasts for time series that are more than 15 years long!\n")
}
if(!is.null(addLines)){ #start option addLines
	header <- vector()
  header[length(header)+1] <- "series {"
  header[length(header)+1] <- 'title="R Output for X12a"'
  header[length(header)+1] <- paste("decimals=",decimals,sep="")
  header[length(header)+1] <- paste("start=",paste(start(tso),collapse="."),sep="")
if(!is.null(span)){
topaste<-span
tocollapse<-c(".",".")
	if(any(is.na(span))){
	topaste[which(is.na(span))]<-""
	tocollapse[which(is.na(span))[2]/2]<-""
	}
	header[length(header)+1] <- paste("span=(",paste(topaste[1:2],collapse=tocollapse[1]),",",paste(topaste[3:4],collapse=tocollapse[2]),")",sep="")
}
if(!is.null(modelspan)){
topaste<-modelspan
tocollapse<-c(".",".")
			if(any(is.na(modelspan))){
				topaste[which(is.na(modelspan))]<-""
				tocollapse[which(is.na(modelspan))[2]/2]<-""
			}
	header[length(header)+1] <- paste("modelspan=(",paste(topaste[1:2],collapse=tocollapse[1]),",",paste(topaste[3:4],collapse=tocollapse[2]),")",sep="")
}
header[length(header)+1] <- paste("period=",period,sep="")
  header[length(header)+1] <- "data=("
  datarows<-vector()
  tsov<-as.vector(tso)
   for(i in 0:(length(tso)/period)){
    datarow<-vector()
    for(j in 1:period){
      if(!is.na(tsov[period*i+j])){
        datarow[j] <- tsov[period*i+j]
	  }else
	    datarow[j] <- ""
    }
    datarows[i+1] <- paste("    ",paste(datarow,collapse=" "),sep="")
  }
  datarows[length(datarows)+1] <- "      )"
	if(any(grepl("series",addLines))){
	if(length(unlist(addLines[which(grepl("series",addLines))]))>1){
		newLines <- unlist(addLines[which(grepl("series",addLines))])[-1]}
	else{
		newLines <- unlist(addLines)[-1]}	
	addhead<-length(datarows)
	for(j in 1:length(newLines)){
		addhead <- addhead+1
	datarows[addhead] <- newLines[j]	
	}
}  
datarows[length(datarows)+1] <- "}"
  addcommands <- vector()
	if(!x11regress){#transform ausschalten falls x11 Regression
  	addcommands[length(addcommands)+1] <- paste("transform {") 
	addcommands[length(addcommands)+1] <- paste("function=",transform,sep="") 
	if(any(grepl("transform",addLines))){
		if(length(unlist(addLines[which(grepl("transform",addLines))]))>1){
			newLines <- unlist(addLines[which(grepl("transform",addLines))])[-1]}
		else{
			newLines <- unlist(addLines)[-1]}	
		length.addcom<- length(addcommands)
		for(j in 1:length(newLines)){
			length.addcom <- length.addcom+1
			addcommands[length.addcom] <- newLines[j]	
		}}  
	addcommands[length(addcommands)+1] <- "}"
	}
  if(!is.null(arima)&&!x11regress){
	arima <- paste("(",paste(arima,collapse=","),")",sep="")
	if(!is.null(sarima))
	sarima <- paste("(",paste(sarima,collapse=","),")",sep="")
    addcommands[length(addcommands)+1] <- paste("arima {")
	addcommands[length(addcommands)+1] <- paste("model=",arima,sarima,sep="")
	if(any(grepl("arima",addLines))){
		if(length(unlist(addLines[which(grepl("arima",addLines))]))>1){
			newLines <- unlist(addLines[which(grepl("arima",addLines))])[-1]}
		else{
			newLines <- unlist(addLines)[-1]}	
		length.addcom<- length(addcommands)
		for(j in 1:length(newLines)){
			length.addcom <- length.addcom+1
			addcommands[length.addcom] <- newLines[j]	
		}}  
	addcommands[length(addcommands)+1] <- "}"
  }
  if(!is.null(arima)&&automdl&&!x11regress)
	  cat("Warning: 'automdl' is ignored because an ARIMA model has been specified! \n")
  
    #cat("Arima and Sarima model specifications are ignored, because automdl is activated! \n")
  if(any(c(!is.null(regvariables),!is.null(reguser),!is.null(regfile))) &&! x11regress){
    addcommands[length(addcommands)+1] <- "regression {"
    if(!is.null(regvariables))
      addcommands[length(addcommands)+1] <- paste("variables=(",paste(regvariables,collapse=" "),")",sep="")
    if(!is.null(aictest))
      addcommands[length(addcommands)+1] <- paste("aictest=(",aictest,") savelog= aictest",sep="")
    if(!is.null(reguser))
      addcommands[length(addcommands)+1] <- paste("user=(",paste(reguser,collapse=" "),")",sep="")
    if(!is.null(regfile))
      addcommands[length(addcommands)+1] <- paste("file='",regfile,"'",sep="")
  if(!is.null(regfilestart))
	  addcommands[length(addcommands)+1] <- paste("start=",paste(regfilestart,collapse="."),"",sep="")
  	if(!is.null(usertype))
	  addcommands[length(addcommands)+1] <- paste("usertype=(",paste(usertype,collapse=" "),")",sep="")
  	if(!is.null(centeruser))
	  addcommands[length(addcommands)+1] <- paste("centeruser=",centeruser,sep="")
  if(any(grepl("regression",addLines))){
	  if(length(unlist(addLines[which(grepl("regression",addLines))]))>1){
		  newLines <- unlist(addLines[which(grepl("regression",addLines))])[-1]}
	  else{
		  newLines <- unlist(addLines)[-1]}	
	  length.addcom<- length(addcommands)
	  	  for(j in 1:length(newLines)){
		  length.addcom <- length.addcom+1
		  addcommands[length.addcom] <- newLines[j]	
		}}  
  	  addcommands[length(addcommands)+1] <- "}"
  }
  if(!is.null(outlier) &&! x11regress){
    addcommands[length(addcommands)+1] <- "outlier {"
    if(all(outlier=="all"))
	  addcommands[length(addcommands)+1] <- "types=(all)"
    else
	  addcommands[length(addcommands)+1] <- paste("types=(",paste(outlier,collapse=" "),")",sep="")
    if(!is.null(critical)){
		if(is.list(critical)){
		critval <- vector()
		ifelse(is.null(critical$AO),critval[1] <- "",critval[1] <- critical$AO)
		ifelse(is.null(critical$LS),critval[2] <- "",critval[2] <- critical$LS)
		ifelse(is.null(critical$TC),critval[3] <- "",critval[3] <- critical$TC)
		addcommands[length(addcommands)+1] <- paste("critical=(",paste(critval,collapse=","),")",sep="")
	}else{addcommands[length(addcommands)+1] <- paste("critical=(",paste(critical,collapse=","),")",sep="")	
	}
	}
  	if(!is.null(outlier_span))
      addcommands[length(addcommands)+1] <- paste("span=(",paste(outlier_span,collapse=","),")",sep="")	
	addcommands[length(addcommands)+1] <- "print=(default)"
	if(!is.null(outlier_method) &&! x11regress){
		addcommands[length(addcommands)+1] <- paste("method=",paste(outlier_method,collapse=","),sep="")	
	}
	if(any(grepl("outlier",addLines))){
		if(length(unlist(addLines[which(grepl("outlier",addLines))]))>1){
			newLines <- unlist(addLines[which(grepl("outlier",addLines))])[-1]}
		else{
			newLines <- unlist(addLines)[-1]}	
		length.addcom<- length(addcommands)
		for(j in 1:length(newLines)){
			length.addcom <- length.addcom+1
			addcommands[length.addcom] <- newLines[j]	
		}}  	
	addcommands[length(addcommands)+1] <- "}"
  }
	if(!x11regress){#nicht bei x11 Regression
  if(estimate){
    addcommands[length(addcommands)+1] <- "estimate {"
#	if(!is.null(estExact)){
#		addcommands[length(addcommands)+1] <- paste("exact=",estExact,sep="")	
##		addcommands[length(addcommands)+1] <- paste("save=(rts)")	
#	}
	if(estOutofsample){	
	addcommands[length(addcommands)+1] <- "outofsample=yes"}
	if(any(grepl("estimate",addLines))){
		if(length(unlist(addLines[which(grepl("estimate",addLines))]))>1){
			newLines <- unlist(addLines[which(grepl("estimate",addLines))])[-1]}
		else{
			newLines <- unlist(addLines)[-1]}	
		length.addcom<- length(addcommands)
	for(j in 1:length(newLines)){
		length.addcom <- length.addcom+1
		addcommands[length.addcom] <- newLines[j]	
	}}  	
    addcommands[length(addcommands)+1] <- "print=(default + rts)"
    addcommands[length(addcommands)+1] <- "savelog=(aic bic afc)"
	addcommands[length(addcommands)+1] <- "}"
    addcommands[length(addcommands)+1] <- "check {"
    addcommands[length(addcommands)+1] <- "print=(default+specresidual+pacfplot)"
    addcommands[length(addcommands)+1] <- "savelog=(nrm lbq)"
	if(any(grepl("check",addLines))){
		if(length(unlist(addLines[which(grepl("check",addLines))]))>1){
			newLines <- unlist(addLines[which(grepl("check",addLines))])[-1]}
		else{
			newLines <- unlist(addLines)[-1]}	
		length.addcom<- length(addcommands)
		for(j in 1:length(newLines)){
			length.addcom <- length.addcom+1
			addcommands[length.addcom] <- newLines[j]	
		}}  	
	addcommands[length(addcommands)+1] <- "}"
  }
  if(slidingspans){
    addcommands[length(addcommands)+1] <- "slidingspans {" 
    addcommands[length(addcommands)+1] <- "savelog=percent"
    addcommands[length(addcommands)+1] <- "additivesa=percent"
	if(any(grepl("slidingspans",addLines))){
		if(length(unlist(addLines[which(grepl("slidingspans",addLines))]))>1){
			newLines <- unlist(addLines[which(grepl("slidingspans",addLines))])[-1]}
		else{
			newLines <- unlist(addLines)[-1]}	
		length.addcom<- length(addcommands)
		for(j in 1:length(newLines)){
			length.addcom <- length.addcom+1
			addcommands[length.addcom] <- newLines[j]	
		}}  	
    addcommands[length(addcommands)+1] <- "}" 
  }
    if(automdl && is.null(arima) && is.null(sarima)){
  addcommands[length(addcommands)+1] <- "automdl {"
  if(acceptdefault)
    addcommands[length(addcommands)+1] <- "acceptdefault=yes"
  else
    addcommands[length(addcommands)+1] <- "acceptdefault=no"
  if(balanced)
    addcommands[length(addcommands)+1] <- "balanced=yes"
  else
    addcommands[length(addcommands)+1] <- "balanced=no"

  addcommands[length(addcommands)+1] <- paste("maxorder=(",paste(maxorder,collapse=","),")",sep="")
  addcommands[length(addcommands)+1] <- paste("maxdiff=(",paste(maxdiff,collapse=","),")",sep="")
  addcommands[length(addcommands)+1] <- "balanced=yes"
  if(any(grepl("automdl",addLines))){
	  if(length(unlist(addLines[which(grepl("automdl",addLines))]))>1){
		  newLines <- unlist(addLines[which(grepl("automdl",addLines))])[-1]}
	  else{
		  newLines <- unlist(addLines)[-1]}	
	  length.addcom<- length(addcommands)
	  for(j in 1:length(newLines)){
		  length.addcom <- length.addcom+1
		  addcommands[length.addcom] <- newLines[j]	
	  }}  	
  addcommands[length(addcommands)+1] <- "savelog=(adf amd b5m mu)"
  addcommands[length(addcommands)+1] <- "}" }
#Forecasts Backcasts
  addcommands[length(addcommands)+1] <- "forecast {"
  addcommands[length(addcommands)+1] <- paste("PROBABILITY=",forecast_conf,sep="")
	if(!is.null(forecast_years)){
		addcommands[length(addcommands)+1] <- paste("maxlead=",forecast_years*frequency(tso),sep="")
	}
	if(!is.null(backcast_years)){
		addcommands[length(addcommands)+1] <- paste("maxback=",backcast_years*frequency(tso),sep="")
#	save(bct)
	}
	if(any(grepl("forecast",addLines))){
		if(length(unlist(addLines[which(grepl("forecast",addLines))]))>1){
			newLines <- unlist(addLines[which(grepl("forecast",addLines))])[-1]}
		else{
			newLines <- unlist(addLines)[-1]}	
		length.addcom<- length(addcommands)
		for(j in 1:length(newLines)){
			length.addcom <- length.addcom+1
			addcommands[length.addcom] <- newLines[j]	
		}}  	
	addcommands[length(addcommands)+1] <- "}"
  
  }#end nicht bei x11 Regression
  if(!seats){
	  addcommands[length(addcommands)+1] <- "x11 {"
	  if(onlytd)
		  addcommands[length(addcommands)+1] <- "type= trend"
	  if(sfshort)  
		  addcommands[length(addcommands)+1] <- "sfshort=yes"
	  if(!is.null(sigmalim)){
		  sigmalim <- paste("(",sigmalim[1],",",sigmalim[2],")",sep="")
		  addcommands[length(addcommands)+1] <- paste("sigmalim=",sigmalim,sep="")
	  }
	  if(!is.null(samode))
	addcommands[length(addcommands)+1] <- paste("mode=",samode,sep="")	
	if(!is.null(seasonalma)){
	addcommands[length(addcommands)+1] <- paste("seasonalma=(",paste(seasonalma,collapse=" "),")",sep="")}	
	if(!is.null(trendma)){
	addcommands[length(addcommands)+1] <- paste("trendma=",trendma,sep="")		
	}
	#x11appendbcst=TRUE,x11appendbcst=FALSE,x11calendarsigma=NULL,x11excludefcst=TRUE,x11final="user",
	if(!is.null(x11calendarsigma))
	addcommands[length(addcommands)+1] <- paste("calendarsigma=",x11calendarsigma,sep="")
	if(x11excludefcst)
	addcommands[length(addcommands)+1] <- "excludefcst=yes"
	if(x11appendbcst)
	addcommands[length(addcommands)+1] <- "appendbcst=yes" ###backcast
	if(x11final!="none")
	addcommands[length(addcommands)+1] <- paste("final=(",paste(x11final,collapse=" "),")",sep="")
	if(x11appendfcst)
	addcommands[length(addcommands)+1] <- "appendfcst=yes" ###forecast
	if(any(grepl("x11",addLines))){
		if(length(unlist(addLines[which(grepl("x11",addLines))]))>1){
		newLines <- unlist(addLines[which(grepl("x11",addLines))])[-1]}
		else{
		newLines <- unlist(addLines)[-1]}
		length.addcom<- length(addcommands)
		for(j in 1:length(newLines)){
			length.addcom <- length.addcom+1
			addcommands[length.addcom] <- newLines[j]	
		}}  	
    addcommands[length(addcommands)+1] <- "savelog=all"
    addcommands[length(addcommands)+1] <- "}" 
  }else{
    addcommands[length(addcommands)+1] <- paste("seats {")
	addcommands[length(addcommands)+1] <- paste(seatsparameter)	
	if(any(grepl("seats",addLines))){
		if(length(unlist(addLines[which(grepl("seats",addLines))]))>1){
			newLines <- unlist(addLines[which(grepl("seats",addLines))])[-1]}
		else{
			newLines <- unlist(addLines)[-1]}	
		length.addcom<- length(addcommands)
		for(j in 1:length(newLines)){
			length.addcom <- length.addcom+1
			addcommands[length.addcom] <- newLines[j]	
		}}  		
	addcommands[length(addcommands)+1] <- "}"
	}
  if(x11regress){
#start: The start date for the values of the user-defined regression variables.
# The default is the start date of the series. 
# Valid values are any date up to the start date of the series 
# (or up to the start date of the span specified by the span argument of the series spec, if present).
	  addcommands[length(addcommands)+1] <- "x11regression {"
	  if(!is.null(regfilestart))
	  addcommands[length(addcommands)+1] <- paste("start=",paste(regfilestart,collapse="."),sep="")
		else
	  addcommands[length(addcommands)+1] <- paste("start=",paste(start(tso),collapse="."),sep="")
	if(!is.null(critical)){
		if(is.list(critical) & length(critical)>1 &!"AO"%in%names(critical)){
			cat("X11 Regression only allows for the detection of Additive Outliers (AO)! \n")}
		else
			addcommands[length(addcommands)+1] <- paste("critical=",critical,sep="")	
	}		
	if(!is.null(outlier_method)){
		addcommands[length(addcommands)+1] <- paste("outliermethod=",paste(outlier_method,collapse=","),sep="")	
	}
	if(!is.null(regvariables))
		addcommands[length(addcommands)+1] <- paste("variables=(",paste(regvariables,collapse=" "),")",sep="")
	if(!is.null(reguser))
		addcommands[length(addcommands)+1] <- paste("user=(",paste(reguser,collapse=" "),")",sep="")
	if(!is.null(regfile))
		addcommands[length(addcommands)+1] <- paste("file='",regfile,"'",sep="")				
	if(!is.null(centeruser))
	addcommands[length(addcommands)+1] <- paste("centeruser=",centeruser,sep="")
	if(!is.null(usertype))
	addcommands[length(addcommands)+1] <- paste("usertype=(",paste(usertype,collapse=" "),")",sep="")
	if(any(grepl("x11regression",addLines))){
		if(length(unlist(addLines[which(grepl("x11regression",addLines))]))>1){
			newLines <- unlist(addLines[which(grepl("x11regression",addLines))])[-1]}
		else{
			newLines <- unlist(addLines)[-1]}	
		length.addcom<- length(addcommands)
	for(j in 1:length(newLines)){
		length.addcom <- length.addcom+1
		addcommands[length.addcom] <- newLines[j]	
	}}  	
	  addcommands[length(addcommands)+1] <- "}"	
  
	  }#end x11regress
if(length(unlist(addLines[1]))>1){
new.spec <- which(!sapply(addLines,function(x)any(grepl(paste(x[[1]],"{"),c(header,addcommands),fixed=TRUE))))
}else{
new.spec <- which(!paste(addLines[1],"{")%in%c(header,addcommands))	
}
if(length(new.spec)>0){
for(i in new.spec){
	addcommands[length(addcommands)+1] <- paste(unlist(addLines[i])[1],"{")
	if(length(unlist(addLines[i]))>1){
	newLines <- unlist(addLines[i])[-1]}
	else{
	newLines <- unlist(addLines)[-1]	
	}
	length.addcom<- length(addcommands)
	for(j in 1:length(newLines)){
	length.addcom <- length.addcom +1
	addcommands[length.addcom] <- newLines[j]	
	}
	addcommands[length(addcommands)+1] <- "}"
}}
}else{#start option no addLines
	  
	  header <- vector()
	  header[length(header)+1] <- "series{"
	  header[length(header)+1] <- 'title="R Output for X12a"'
	  header[length(header)+1] <- paste("decimals=",decimals,sep="")
	  header[length(header)+1] <- paste("start=",paste(start(tso),collapse="."),sep="")
	  if(!is.null(span)){
		  topaste<-span
		  tocollapse<-c(".",".")
		  if(any(is.na(span))){
			  topaste[which(is.na(span))]<-""
			  tocollapse[which(is.na(span))[2]/2]<-""
		  }
		  header[length(header)+1] <- paste("span=(",paste(topaste[1:2],collapse=tocollapse[1]),",",paste(topaste[3:4],collapse=tocollapse[2]),")",sep="")
	  }
	  if(!is.null(modelspan)){
		  topaste<-modelspan
		  tocollapse<-c(".",".")
		  if(any(is.na(modelspan))){
			  topaste[which(is.na(modelspan))]<-""
			  tocollapse[which(is.na(modelspan))[2]/2]<-""
		  }
		  header[length(header)+1] <- paste("modelspan=(",paste(topaste[1:2],collapse=tocollapse[1]),",",paste(topaste[3:4],collapse=tocollapse[2]),")",sep="")
	  }
	  header[length(header)+1] <- paste("period=",period,sep="")
	  header[length(header)+1] <- "data=("
	  datarows<-vector()
	  tsov<-as.vector(tso)
	  for(i in 0:(length(tso)/period)){
		  datarow<-vector()
		  for(j in 1:period){
			  if(!is.na(tsov[period*i+j])){
				  datarow[j] <- tsov[period*i+j]
			  }else
				  datarow[j] <- ""
		  }
		  datarows[i+1] <- paste("    ",paste(datarow,collapse=" "),sep="")
	  }
	  datarows[length(datarows)+1] <- "      )"
	  datarows[length(datarows)+1] <- "}"
	  addcommands <- vector()
	  if(!x11regress){#transform ausschalten falls x11 Regression
		  addcommands[length(addcommands)+1] <- paste("transform{") 
	  addcommands[length(addcommands)+1] <- paste("function=",transform,sep="") 
	  addcommands[length(addcommands)+1] <- "}"
  	}
	  if(!is.null(arima)&&!x11regress){
		  arima <- paste("(",paste(arima,collapse=","),")",sep="")
		  if(!is.null(sarima))  
		  sarima <- paste("(",paste(sarima,collapse=","),")",sep="")
		  addcommands[length(addcommands)+1] <- paste("arima{")
		  addcommands[length(addcommands)+1] <- paste("model=",arima,sarima,sep="")
		  addcommands[length(addcommands)+1] <- "}"
	  }
	  if(!is.null(arima)&&automdl&&!x11regress)
		  cat("Warning: 'automdl' is ignored because an ARIMA model has been specified! \n")
		  #cat("Arima and Sarima model specifications are ignored, because automdl is activated! \n")
	  if(any(c(!is.null(regvariables),!is.null(reguser),!is.null(regfile))) &&! x11regress){
		  addcommands[length(addcommands)+1] <- "regression{"
		  if(!is.null(regvariables))
			  addcommands[length(addcommands)+1] <- paste("variables=(",paste(regvariables,collapse=" "),")",sep="")
		  if(!is.null(aictest))
			  addcommands[length(addcommands)+1] <- paste("aictest=(",aictest,") savelog= aictest",sep="")
		  if(!is.null(reguser))
			  addcommands[length(addcommands)+1] <- paste("user=(",paste(reguser,collapse=" "),")",sep="")
		  if(!is.null(regfile))
			  addcommands[length(addcommands)+1] <- paste("file='",regfile,"'",sep="")
		  if(!is.null(regfilestart))
			  addcommands[length(addcommands)+1] <- paste("start=",paste(regfilestart,collapse="."),"",sep="")
		  if(!is.null(usertype))
			  addcommands[length(addcommands)+1] <- paste("usertype=(",paste(usertype,collapse=" "),")",sep="")
		  if(!is.null(centeruser))
			  addcommands[length(addcommands)+1] <- paste("centeruser=",centeruser,sep="")
		  addcommands[length(addcommands)+1] <- "}"
	  }
	  if(!is.null(outlier) &&! x11regress){
		  addcommands[length(addcommands)+1] <- "outlier {"
		  if(all(outlier=="all"))
			  addcommands[length(addcommands)+1] <- "types=(all)"
		  else
			  addcommands[length(addcommands)+1] <- paste("types=(",paste(outlier,collapse=" "),")",sep="")
		  if(!is.null(critical)){
			  if(is.list(critical)){
				  critval <- vector()
				  ifelse(is.null(critical$AO),critval[1] <- "",critval[1] <- critical$AO)
				  ifelse(is.null(critical$LS),critval[2] <- "",critval[2] <- critical$LS)
				  ifelse(is.null(critical$TC),critval[3] <- "",critval[3] <- critical$TC)
				  addcommands[length(addcommands)+1] <- paste("critical=(",paste(critval,collapse=","),")",sep="")
			  }else{addcommands[length(addcommands)+1] <- paste("critical=(",paste(critical,collapse=","),")",sep="")	
			  }
		  }
		  if(!is.null(outlier_span))
			  addcommands[length(addcommands)+1] <- paste("span=(",paste(outlier_span,collapse=","),")",sep="")	
		  addcommands[length(addcommands)+1] <- "print=(default)"
		  if(!is.null(outlier_method) &&! x11regress){
			  addcommands[length(addcommands)+1] <- paste("method=",paste(outlier_method,collapse=","),sep="")	
		  }
		  addcommands[length(addcommands)+1] <- "}"
	  }
	  if(!x11regress){#nicht bei x11 Regression
		  if(estimate){
			  addcommands[length(addcommands)+1] <- "estimate {"
#	if(!is.null(estExact)){
#		addcommands[length(addcommands)+1] <- paste("exact=",estExact,sep="")	
			  ##		addcommands[length(addcommands)+1] <- paste("save=(rts)")	
#	}
			  if(estOutofsample){	
				  addcommands[length(addcommands)+1] <- "outofsample=yes"}
			  addcommands[length(addcommands)+1] <- "print=(default + rts)"
			  addcommands[length(addcommands)+1] <- "savelog=(aic bic afc)"
			  addcommands[length(addcommands)+1] <- "}"
			  addcommands[length(addcommands)+1] <- "check{"
			  addcommands[length(addcommands)+1] <- "print=(default+specresidual+pacfplot)"
			  addcommands[length(addcommands)+1] <- "savelog=(nrm lbq)"
			  addcommands[length(addcommands)+1] <- "}"
		  }
		  if(slidingspans){
			  addcommands[length(addcommands)+1] <- "slidingspans{" 
			  addcommands[length(addcommands)+1] <- "savelog= percent"
			  addcommands[length(addcommands)+1] <- "additivesa= percent"
			  addcommands[length(addcommands)+1] <- "}" 
		  }
		  if(automdl && is.null(arima) && is.null(sarima)){
			  addcommands[length(addcommands)+1] <- "automdl {"
#			  addcommands[length(addcommands)+1] <- "#acceptdefault=yes"
			  addcommands[length(addcommands)+1] <- paste("maxorder=(",paste(maxorder,collapse=","),")",sep="")
			  addcommands[length(addcommands)+1] <- paste("maxdiff=(",paste(maxdiff,collapse=","),")",sep="")
			  addcommands[length(addcommands)+1] <- "balanced=yes"
			  addcommands[length(addcommands)+1] <- "savelog=(adf amd b5m mu)"
			  addcommands[length(addcommands)+1] <- "}" }
#Forecasts Backcasts
		  addcommands[length(addcommands)+1] <- "forecast {"
		  if(!is.null(forecast_years)){
			  addcommands[length(addcommands)+1] <- paste("maxlead=",forecast_years*frequency(tso),sep="")
		  }
		  if(!is.null(backcast_years)){
			  addcommands[length(addcommands)+1] <- paste("maxback=",backcast_years*frequency(tso),sep="")
#	save(bct)
		  }
		  addcommands[length(addcommands)+1] <- "}"
		  
	  }#end nicht bei x11 Regression
	  if(!seats){
		  addcommands[length(addcommands)+1] <- "x11{"
		  if(onlytd)
			  addcommands[length(addcommands)+1] <- "type= trend"
		  if(sfshort)  
			  addcommands[length(addcommands)+1] <- "sfshort=yes"
		  if(!is.null(sigmalim)){
			  sigmalim <- paste("(",sigmalim[1],",",sigmalim[2],")",sep="")
			  addcommands[length(addcommands)+1] <- paste("sigmalim=",sigmalim,sep="")
		  }
		  if(!is.null(samode))
			  addcommands[length(addcommands)+1] <- paste("mode=",samode,sep="")	
		  if(!is.null(seasonalma)){
			  addcommands[length(addcommands)+1] <- paste("seasonalma=(",paste(seasonalma,collapse=" "),")",sep="")}	
		  if(!is.null(trendma)){
			  addcommands[length(addcommands)+1] <- paste("trendma=",trendma,sep="")		
		  }
#x11appendbcst=TRUE,x11appendbcst=FALSE,x11calendarsigma=NULL,x11excludefcst=TRUE,x11final="user",
		  if(!is.null(x11calendarsigma))
			  addcommands[length(addcommands)+1] <- paste("calendarsigma=",x11calendarsigma,sep="")
		  if(x11excludefcst)
			  addcommands[length(addcommands)+1] <- "excludefcst=yes"
		  if(x11appendbcst)
			  addcommands[length(addcommands)+1] <- "appendbcst=yes" ###backcast
		  if(x11final!="none")
			  addcommands[length(addcommands)+1] <- paste("final=(",paste(x11final,collapse=" "),")",sep="")
		  if(x11appendfcst)
			  addcommands[length(addcommands)+1] <- "appendfcst=yes" ###forecast		  
#		  addcommands[length(addcommands)+1] <- "calendarsigma=all"
#		  addcommands[length(addcommands)+1] <- "excludefcst=yes"
#		  addcommands[length(addcommands)+1] <- "final=user"
#		  addcommands[length(addcommands)+1] <- "appendfcst=yes" ###forecast
		  addcommands[length(addcommands)+1] <- "savelog=all"
		  addcommands[length(addcommands)+1] <- "}" 
	  }else{
		  addcommands[length(addcommands)+1] <- paste("seats{",seatsparameter,"}",sep="")
	  }
	  if(x11regress){
#start: The start date for the values of the user-defined regression variables.
# The default is the start date of the series. 
# Valid values are any date up to the start date of the series 
# (or up to the start date of the span specified by the span argument of the series spec, if present).
		  addcommands[length(addcommands)+1] <- "x11regression{"
		  if(!is.null(regfilestart))
			  addcommands[length(addcommands)+1] <- paste("start=",paste(regfilestart,collapse="."),sep="")
		  else
			  addcommands[length(addcommands)+1] <- paste("start=",paste(start(tso),collapse="."),sep="")
		  if(!is.null(critical)){
			  if(is.list(critical) & length(critical)>1 &!"AO"%in%names(critical)){
				  cat("X11 Regression only allows for the detection of Additive Outliers (AO)! \n")}
			  else
				  addcommands[length(addcommands)+1] <- paste("critical=",critical,sep="")	
		  }		
		  if(!is.null(outlier_method)){
			  addcommands[length(addcommands)+1] <- paste("outliermethod=",paste(outlier_method,collapse=","),sep="")	
		  }
		  if(!is.null(regvariables))
			  addcommands[length(addcommands)+1] <- paste("variables=(",paste(regvariables,collapse=" "),")",sep="")
		  if(!is.null(reguser))
			  addcommands[length(addcommands)+1] <- paste("user=(",paste(reguser,collapse=" "),")",sep="")
		  if(!is.null(regfile))
			  addcommands[length(addcommands)+1] <- paste("file='",regfile,"'",sep="")				
		  if(!is.null(centeruser))
			  addcommands[length(addcommands)+1] <- paste("centeruser=",centeruser,sep="")
		  if(!is.null(usertype))
			  addcommands[length(addcommands)+1] <- paste("usertype=(",paste(usertype,collapse=" "),")",sep="")
		  addcommands[length(addcommands)+1] <- "}"	
		  
	  }
	  
}#end option no addLines
  con <- file(paste(file,".spc",sep=""))
#  if(!is.null(addLines))
#    addcommands <- c(addcommands,addLines)
  writeLines(c(header,datarows,addcommands),con)
  close(con)

  
  if(Sys.info()[1]=="Windows"){
    con1 <- file("run.bat")
    mdcommand <- "md gra"
	file_1 <- gsub("/","\\\\",file)
    if((!is.null(x12path)) && use=="x12"){
      x12path_1 <- gsub("/","\\\\",x12path)
      command <- paste(x12path_1," ",file_1," -g gra",sep="")
    }else if((!is.null(x13path)) && use!="x12"){
      x13path_1 <- gsub("/","\\\\",x13path)
      command <- paste(x13path_1," ",file_1," -g gra",sep="")
    }else 
      stop("Please define the path to the X12 binaries!")
  }else{
    con1 <- file("run.sh")
    mdcommand <- "mkdir gra"
    if((!is.null(x12path)) && use=="x12"){
      command <- paste(x12path," ",file," -g gra",sep="")
    }else if((!is.null(x13path)) && use!="x12"){
      command <- paste(x13path," ",file," -g gra",sep="")
    }else
      stop("Please define the path to the X12 binaries!")
  }
  writeLines(c(mdcommand,command),con1)
  close(con1)
  if(Sys.info()[1]=="Windows")
    system("run.bat")
  else{
    system("chmod 744 run.sh")
    system("./run.sh")
  }

#  out <- list()


  out <- readx12Out(file,freq_series=frequency(tso),start_series=start(tso),end_series=end(tso),tblnames=tblnames,Rtblnames=Rtblnames,transform=transform,x11regress=x11regress,outlier=outlier,showWarnings=showWarnings,keep_x12out=keep_x12out)
#  Rtblnames <- c("Original series", "Final seasonal factors", "Final seasonally adjusted data", "Final trend cycle",
#		    "Final irregular components","Combined adjustment factors","Final weights for irregular component",
#			"Final replacements for SI ratios",
#			"Differenced, transformed, seasonally adjusted data",
#			Rtblnames)
#  if(seats==TRUE)
#    tblnames <- c("a1", "s10", "s11", "s12", "s13","s16","c17","s9","e2", tblnames)
#  else
#    tblnames <- c("a1", "d10", "d11", "d12", "d13","d16","c17","d9","e2", tblnames)
#  for(i in 1:length(tblnames)){
#    if(file.exists(paste("gra\\",file,".",tblnames[i],sep="")))
#      out[[tblnames[i]]] <- ts(read.table(paste("gra\\",file,".",tblnames[i],sep=""),header=FALSE,skip=2,sep="	",na.strings="-999")[,2],frequency=frequency(tso),start=start(tso))
#  }
#  spnames <- c("Spectrum_AdjOri","Spectrum_SA","Spectrum_Irr")
#  sptblnames <- c("sp0", "sp1", "sp2")
#  if(!seats){
#    for(i in 1:length(sptblnames)){
#      out[[spnames[i]]] <- read.table(paste("gra\\",file,".",sptblnames[i],sep=""),header=FALSE,skip=2,sep="	")[,2:3]
#      names(out[[spnames[i]]]) <- c("frequency","spectrum")
#    }
#  }
#  out[["d9"]][out[["d9"]]==-999]<-NA
#  out[["Forecast with CI"]] <- list()
#  fct <- read.table(paste("gra\\",file,".","fct",sep=""),header=FALSE,skip=2,sep="	")
#  out[["Forecast with CI"]]$estimate <-ts(fct[,2],frequency=frequency(tso),start=end(tso)) 
#  out[["Forecast with CI"]]$lower <-ts(fct[,3],frequency=frequency(tso),start=end(tso))
#  out[["Forecast with CI"]]$upper <-ts(fct[,4],frequency=frequency(tso),start=end(tso))
#  out$seats <- seats
#  out$file <- file
#  out$tblnames <- tblnames
#  out$Rtblnames <- Rtblnames
#  class(out) <- "x12"

#file.remove(grep(basename(file),list.files(dirname(file)),value=TRUE))

if(!keep_x12out)
unlink(paste(dirname(file),"/gra",sep=""),recursive=TRUE)
if(file.exists("run.bat"))
file.remove("run.bat")
if(file.exists("run.sh"))
file.remove("run.sh")	
if(file.exists(paste(basename(file),".err",sep=""))){ 
if(!keep_x12out)
file.remove(paste(basename(file),".err",sep=""))
}
out

}

readx12Out <- function(file,tblnames=NULL,Rtblnames=NULL,freq_series,start_series,end_series,seats=FALSE,transform,x11regress,outlier,showWarnings,keep_x12out){
  out<-list()
  Rtblnames <- c("Original series", "Final seasonal factors", "Final seasonally adjusted data", "Final trend cycle",
      "Final irregular components","Combined adjustment factors","Final weights for irregular component",
      "Final replacements for SI ratios",
      "Differenced, transformed, seasonally adjusted data","Final unmodified SI Ratios","Orig2",
      Rtblnames)
  if(seats==TRUE)
    tblnames <- c("a1", "s10", "s11", "s12", "s13","s16","c17","s9","e2","d8","b1", tblnames)
  else
    tblnames <- c("a1", "d10", "d11", "d12", "d13","d16","c17","d9","e2","d8","b1", tblnames)
  if(!(file=="Example_for_X1")){
    sp_file <- strsplit(file,"/")[[1]]
    filename <- paste(paste(sp_file[-length(sp_file)],collapse="/"),"/gra/",sp_file[length(sp_file)],sep="")
    if(substring(filename,1,1)=="/")
      filename <- substring(filename,2)
  }else
    filename <- paste(searchpaths()[grep("x12",searchpaths())],"/doc/Rout",sep="")
############  

#if(file.exists(paste(file,".","err",sep="")) && file.exists(paste(file,".","spc",sep=""))){#evt unnoetige Abfrage

#	x1<-	file.info(paste(filename,".","udg",sep=""))
#	cat("udg:",str(x1),"\n")
#	x2<-	file.info(paste(file,".","err",sep=""))
#		cat("err:",str(x2),"\n")
#	ind<-	!identical(x1$mtime,x2$mtime)
#	cat("ind:",ind,"\n")	
#		udgtime<-file.info(paste(filename,".","udg",sep=""))$mtime
#		errtime<-file.info(paste(file,".","err",sep=""))$mtime
		
#		if(ind){
#			#cat("udg:",file.info(paste(filename,".","udg",sep=""))$mtime,"\n")
#			#cat("err:",file.info(paste(file,".","err",sep=""))$mtime,"\n")
#			errorfile <- readLines(con=paste(file,".","err",sep=""),n=-1)
#			for(i in 1:length(errorfile)){
#				cat(errorfile[i],"\n")	
#			}
#			stop("Error! No proper run of x12! Check your parameter settings.\n=> Beware that files in \"gra\" directory do not represent current x12 output")
#		}
		
if(!file.exists(paste(filename,".","udg",sep=""))){
		
		errorfile <- readLines(con=paste(file,".","err",sep=""),n=-1)
		for(i in 1:length(errorfile)){
			cat(errorfile[i],"\n")	
		}
		if(!keep_x12out)
			unlink(paste(dirname(file),"/gra",sep=""),recursive=TRUE)
		if(file.exists("run.bat"))
			file.remove("run.bat")
		if(file.exists("run.sh"))
			file.remove("run.sh")	
		
		stop("Error! No proper run of x12! Check your parameter settings.")	
	}
	udg <- readLines(con=paste(filename,".","udg",sep=""),n=-1)
#errorfile <- readLines(con=paste(file,".","err",sep=""),n=-1)
#filename <- "M:/Meraner/Workspace/Saisonbereinigung_Test/gra/Rout"
#file<-"M:/Meraner/Workspace/Saisonbereinigung_Test/Rout"

	
	if(showWarnings && file.exists(paste(file,".","err",sep=""))){
		errorfile <- readLines(con=paste(file,".","err",sep=""),n=-1)
			for(i in min(which(errorfile=="")):length(errorfile)){
				cat(errorfile[i],"\n")	
			}
		}
	if(any(any(grepl("errorstop: yes",udg)),length(udg)==0)){
		if(!showWarnings){
			errorfile <- readLines(con=paste(file,".","err",sep=""),n=-1)
			for(i in which(grepl("ERROR:",errorfile)):length(errorfile)){
				cat(errorfile[i],"\n")	
			}
			if(!keep_x12out)
				unlink(paste(dirname(file),"/gra",sep=""),recursive=TRUE)
			if(file.exists("run.bat"))
				file.remove("run.bat")
			if(file.exists("run.sh"))
				file.remove("run.sh")	
			
			stop("An error occured when running x12! Program halted!","\n") 	
		}else{
		if(!keep_x12out)
			unlink(paste(dirname(file),"/gra",sep=""),recursive=TRUE)
		if(file.exists("run.bat"))
			file.remove("run.bat")
		if(file.exists("run.sh"))
			file.remove("run.sh")	
		
		stop("An error occured when running x12! Program halted!","\n") 	
	}
#stop("Gacksnhuaba, error occured when running x12! Program halted!")	
#stop("Errorstop! (Check error file \"",file,".err\")",sep="")	
	}
	
############	
	for(i in 1:length(tblnames)){
    if(file.exists(paste(filename,".",tblnames[i],sep="")))
      out[[tblnames[i]]] <- ts(read.table(paste(filename,".",tblnames[i],sep=""),header=FALSE,skip=2,sep="	",na.strings="-999")[,2],frequency=freq_series,start=start_series)
  }
  if(!x11regress){
  spnames <- c("Spectrum_AdjOri","Spectrum_SA","Spectrum_Irr","Spectrum_Rsd")
  sptblnames <- c("sp0", "sp1", "sp2","spr")
	}else{
	spnames <- c("Spectrum_AdjOri","Spectrum_SA","Spectrum_Irr")
	sptblnames <- c("sp0", "sp1", "sp2")	
	}
  if(!seats){
    for(i in 1:length(sptblnames)){
      out[[sptblnames[i]]] <- read.table(paste(filename,".",sptblnames[i],sep=""),header=FALSE,skip=2,sep="	")[,2:3]
      names(out[[sptblnames[i]]]) <- c("frequency","spectrum")
    }
  }
  out[["d9"]][out[["d9"]]==-999]<-NA
  if(!x11regress){
### Forecasts:	  
if(file.exists(paste(filename,".","fct",sep=""))){
  out[["forecast"]] <- list()
  fct <- read.table(paste(filename,".","fct",sep=""),header=FALSE,skip=2,sep="	")
	  
  if((freq_series==12 && end_series[2]==12) | (freq_series==4 && end_series[2]==4)){
    start_forecast <- c(end_series[1]+1,1)
#  }else if(freq_series==4 && end_series[2]==4){
#	  start_forecast <- c(end_series[1]+1,1)}
	}else{
    start_forecast <- end_series
    start_forecast[2] <- start_forecast[2]+1 
  }
  out[["forecast"]]$estimate <-ts(fct[,2],frequency=freq_series,start=start_forecast) 
  out[["forecast"]]$lowerci <-ts(fct[,3],frequency=freq_series,start=start_forecast)
  out[["forecast"]]$upperci <-ts(fct[,4],frequency=freq_series,start=start_forecast)
}
### Backcasts:
if(file.exists(paste(filename,".","bct",sep=""))){
out[["backcast"]] <- list()
bct <- read.table(paste(filename,".","bct",sep=""),header=FALSE,skip=2,sep="	")
if(start_series[2]==1){
	if(freq_series==12)
	#start_backcast <- c(start_series[1]-1,12)
	end_backcast <- c(start_series[1]-1,12)	
	if(freq_series==4)
		end_backcast <- c(start_series[1]-1,4)	
}else{
	end_backcast <- start_series
	end_backcast[2] <- end_backcast[2]-1 
}
out[["backcast"]]$estimate <-ts(bct[,2],frequency=freq_series,end=end_backcast) 
out[["backcast"]]$lowerci <-ts(bct[,3],frequency=freq_series,end=end_backcast)
out[["backcast"]]$upperci <-ts(bct[,4],frequency=freq_series,end=end_backcast)
}

}
#Testbeispiele:
#filename <- "M:/Meraner/Workspace/Saisonbereinigung_Test/gra/Rout"
#filename <- "M:/Meraner/Saisonbereinigung/x12probierfiles/loge_d"  
#filename <- "M:/Meraner/Saisonbereinigung/x12probierfiles/a"  
#filename <- "M:/Meraner/Saisonbereinigung/x12probierfiles/b05"
#filename <- "M:/Meraner/Workspace/Saisonbereinigung_Test/gra/Rout"
#file<-"M:/Meraner/Workspace/Saisonbereinigung_Test/Rout"


#if(file.exists(paste(filename,".",tblnames[i],sep="")))
# RegARIMA Option:
#wieder entauskommentieren?
#udg <- readLines(con=paste(filename,".","udg",sep=""),n=-1)


if("x11regress: no" %in% udg){
#Informationen die aus udg File eingelesen werden sollen:
dglist <- c("x11regress:","transform:","samode:","finmode:","seasonalma:","trendma:","sfmsr:",
			"arimamdl:","automdl:", 
		  "finalreg",
		  "outlier.total:","autoout:","nalmostout:",
		  "almostoutlier$","crit:",
		  "Outlier$","User-defined$","AutoOutlier$",
		#Autokorrelationen
		  #"sfmsr:","finaltrendma:",
		"peaks.seas:","peaks.td:",  
		  "f2.idseasonal:",
		  "d11.f:",
		  "spcrsd",
		  "spcori",
		  "spcsa",
		  "spcirr",
		  "f3.q:","f3.qm2:","f3.fail:",
		  "loglikelihood:","aic:","aicc:","bic:","hq:","aape")
  
#Hier die gewuenschten Variablennamen eintragen:
  dglistnames <-  c("x11regress","transform","samode","finalsamode","seasonalma","trendma","finalseasonalma",
		  "arimamdl","automdl", 
		  "regmdl",
		  "nout","nautoout","nalmostout",
		  "almostoutlier","crit",
		  "outlier","userdefined","autooutlier",
		  #Autokorrelationen
		  #"sfmsr","finaltrendma",
		  "peaks.seas","peaks.td",  
		  "id.seas",
		  "id.rsdseas",
		  "spcrsd",
		  "spcori",
		  "spcsa",
		  "spcirr",
		  "q","q2","nmfail",
		  "loglikelihood","aic","aicc","bic","hq","aape") 

  if(transform=="auto" &&!x11regress){#x11regress Abfrage hier eigentl nicht mehr notw
	  dglist[length(dglist)+1]<-"aictrans:"
  dglistnames[length(dglistnames)+1]<-"autotransform"
}

  numvariables <- c("nout","nautoout","nalmostout",
		  "crit","spcrsd",
		  "spcori",
		  "spcsa",
		  "spcirr",
		  "q","q2","nmfail","seasonalma","trendma","finalseasonalma",
		  "loglikelihood","aic","aicc","bic","hq","aape")
  dg <- lapply(dglist,function(x)grep(x,udg,value=TRUE,fixed=TRUE))

#Extrawurst fuer Regression variables Teil 1:
if(length(dg[[which(dglist=="finalreg")]])>1){
	dg[[which(dglist=="finalreg")]]<-dg[[which(dglist=="finalreg")]][-grep("nfinalreg",dg[[which(dglist=="finalreg")]])]	
}

  regvar<-unlist(strsplit(dg[[which(dglist=="finalreg")]],": "))
  if(any(grepl("+",regvar,fixed=TRUE))){
  regvar <- strsplit(regvar,"+",fixed=TRUE)
  regvar<-unlist(lapply(regvar,function(x)gsub("^\\s+|\\s+$", "", x))[-(grep("finalreg",regvar))])
  if(""%in%regvar){
	  regvar<-regvar[-which(regvar=="")]}  
	}
  othername<-which(regvar %in% c("User-defined","Automatically Identified Outliers"))
  if(length(othername)>0){
  regvar <- regvar[-othername]}
#End Extrawurst Teil 1
  empty <- which(lapply(dg,function(x)length(x))==0)
  dg[empty] <- paste(gsub("$",replacement=":",dglist[empty],fixed=TRUE),"-")
  dg <- lapply(dg,function(x)strsplit(x,": "))
  names(dg)<- dglistnames
  
  if(length(which(dg[["outlier"]]%in%dg[["autooutlier"]]))>0){
	  dg[["outlier"]]<-dg[["outlier"]][-which(dg[["outlier"]]%in%dg[["autooutlier"]])]}

  if(length(dg[["outlier"]])==0){
	dg["outlier"] <- list(strsplit(paste("outlier","-")," ",fixed=TRUE))
	empty <- c(empty,which(names(dg)=="outlier"))
	}

  grone <- which(lapply(1:length(dg),function(x){length(dg[[x]])})>1)
  grone.nodoll <- unlist(lapply(grep("$",dglist[grone],fixed=TRUE,value=TRUE,invert=TRUE),function(x){
					  grep(x,dglist,fixed=TRUE)}))	
  for(i in c(1:length(dg))[-grone.nodoll]){
#	  cat("i=",i,"\n")
#	  cat("name=",names(dg)[i],"\n")
	  names(dg[[i]])<-lapply(1:length(dg[[i]]),function(x){
				  if(grepl("$",dg[[i]][[x]][1],fixed=TRUE)){
					  gsub(grep("$",dglist[i],fixed=TRUE,value=TRUE),replacement=paste(dglistnames[i],"_",sep=""),dg[[i]][[x]][1],fixed=TRUE)
				  }else{
					  gsub(dg[[i]][[x]][1],replacement=dglistnames[i],dg[[i]][[x]][1],fixed=TRUE)
				  }})
	  for(j in 1:length(dg[[i]])){
		  dg[[i]][[j]] <- dg[[i]][[j]][-1]
		  # returns string w/o leading or trailing whitespace:
		  dg[[i]][[j]] <- gsub("^\\s+|\\s+$", "", dg[[i]][[j]])
	  }}
 
#Extrawurst fuer Regression variables Teil 2:
  if(length(regvar)!=0 &&regvar!="none"){  
	  reglist <- lapply(1:length(regvar),function(x)grep(paste(regvar[x],"$",sep=""),grep(regvar[x],udg,value=TRUE,fixed=TRUE),fixed=TRUE,value=TRUE))
	  reglist <- lapply(reglist,function(x)strsplit(x,"$",fixed=TRUE))
	  if(length(which(sapply(reglist,function(x)!length(x)>0)))!=0){
	  reglist <- reglist[-(which(sapply(reglist,function(x)!length(x)>0)))]}
	if(length(reglist)!=0){ 
	  reglistnames <- vector()
	  for(i in 1:length(reglist)){
		  regsublistnames<-vector()
		  inregvar<-which(sapply(1:length(reglist[[i]]),function(x)!reglist[[i]][[x]][1]%in%regvar))
		  if(length(inregvar)>0){
			  reglist[[i]] <- reglist[[i]][-(inregvar)]
		  }
		  reglistnames[i] <- reglist[[i]][[1]][1]
		  reglist[[i]]<-lapply(1:length(reglist[[i]]),function(x){
					  reglist[[i]][[x]]<-reglist[[i]][[x]][-1]
					  strsplit(reglist[[i]][[x]],": ")})
			regsublistnames <-lapply(1:length(reglist[[i]]),function(x){
						regsublistnames <- reglist[[i]][[x]][[1]][1]
						if("Leap Year"%in%regsublistnames){
							regsublistnames<-gsub("Leap Year",replacement="leapyear",regsublistnames,fixed=TRUE)  
						}
						if("Trading Day"%in%regsublistnames){
							regsublistnames<-gsub("Trading Day",replacement="td",regsublistnames,fixed=TRUE)  
						}
						regsublistnames<-regsublistnames
					})
			#
		  reglist[[i]]<-lapply(1:length(reglist[[i]]),function(x){reglist[[i]][[x]][[1]]<-reglist[[i]][[x]][[1]][-1]
					  reglist[[i]][[x]][[1]] <- gsub("^\\s+|\\s+$", "", reglist[[i]][[x]][[1]])})
		  if("Leap Year"%in%reglistnames){
		    
		   reglistnames<-gsub("Leap Year",replacement="leapyear",reglistnames,fixed=TRUE) 
	      }
		  if("Trading Day"%in%reglistnames){
			  
			  reglistnames<-gsub("Trading Day",replacement="td",reglistnames,fixed=TRUE) 
		  }
		  names(reglist[[i]])<-regsublistnames
	  }
  dg[(length(dg)+1):(length(dg)+length(reglist))]<-reglist
  names(dg)<-dglistnames<-c(dglistnames,reglistnames)
}else{
names(dg)<- dglistnames
reglistnames <- NULL
}}else{
	names(dg)<- dglistnames
	reglistnames <- NULL
}
#End Extrawurst Teil 2 
#Checking for derived parameter estimates:
if(any(grepl("nregderived:",udg,fixed=TRUE))){
	regderived<-grep("nregderived:",udg,value=TRUE,fixed=TRUE)	
	indnrd<-which(udg==regderived)
	nrd<-as.numeric(strsplit(regderived,": ",fixed=TRUE)[[1]][2])
	regder<-udg[(indnrd+1):(indnrd+nrd)]
	derived.coef<-sapply(strsplit(gsub("$",replacement="_",regder,fixed=TRUE),": ",fixed=TRUE),function(x)
				gsub("\\s+",replacement="",x[[1]]))
}
}
#End RegARIMA Option

# x11Regression Option:
  else{
#Informationen die aus udg File eingelesen werden sollen:
	  dglist <- c("x11regress:",
			"samode:","finmode:","seasonalma:","trendma:","sfmsr:",
			"finalxreg","x11irrcrtval:",
			#"$",
			"$AO","User-defined$","Automatically Identified Outliers$",
			#"sfmsr:","finaltrendma:",
			"peaks.seas:","peaks.td:",  
			"f2.idseasonal:","d11.f:",
			"spcori",
			"spcsa",
			"spcirr",
			"f3.q:","f3.qm2:","f3.fail:")
	
#Neue Variablennamen:
	dglistnames <-  c("x11regress",
			"samode","finalsamode","seasonalma","trendma","finalseasonalma",
			"regmdl","crit","outlier","userdefined","autooutlier",
			#Autokorrelationen
			#"sfmsr","finaltrendma",
			"peaks.seas","peaks.td",  
			"id.seas","id.rsdseas",
			"spcori",
			"spcsa",
			"spcirr",
			"q","q2","nmfail") 
	numvariables <- c("crit",
			"spcori",
			"spcsa",
			"spcirr",
			"q","q2","nmfail","seasonalma","trendma","finalseasonalma")
	dg <- lapply(dglist,function(x)grep(x,udg,value=TRUE,fixed=TRUE))
#Extrawurst fuer Regression variables Teil 1:
if(length(dg[[which(dglist=="finalxreg")]])>1){
dg[[which(dglist=="finalxreg")]]<-dg[[which(dglist=="finalxreg")]][-grep("nfinalxreg",dg[[which(dglist=="finalxreg")]])]	
}
	regvar<-unlist(strsplit(dg[[which(dglist=="finalxreg")]],": "))
	if(any(grepl("+",regvar,fixed=TRUE))){
	regvar <- strsplit(regvar,"+",fixed=TRUE)
	regvar<-unlist(lapply(regvar,function(x)gsub("^\\s+|\\s+$", "", x))[-(grep("finalxreg",regvar))])
	if(""%in%regvar){
	regvar<-regvar[-which(regvar=="")]}
	}
	othername<-which(regvar %in% c("User-defined","Automatically Identified Outliers"))
	if(length(othername)>0){
	regvar <- regvar[-othername]}
#End Extrawurst Teil 1
	
	empty <- which(lapply(dg,function(x)length(x))==0)
	dg[empty] <- paste(gsub("$",replacement=":",dglist[empty],fixed=TRUE),"-")
	dg <- lapply(dg,function(x)strsplit(x,": "))
	names(dg)<- dglistnames
	
	if(length(which(dg[["outlier"]]%in%dg[["autooutlier"]]))>0){
	dg[["outlier"]]<-dg[["outlier"]][-which(dg[["outlier"]]%in%dg[["autooutlier"]])]}

grone <- which(lapply(1:length(dg),function(x){length(dg[[x]])})>1)
grone.nodoll <- unlist(lapply(grep("$",dglist[grone],fixed=TRUE,value=TRUE,invert=TRUE),function(x){
			grep(x,dglist,fixed=TRUE)}))	
for(i in c(1:length(dg))[-grone.nodoll]){
	names(dg[[i]])<-lapply(1:length(dg[[i]]),function(x){
	if(grepl("$",dg[[i]][[x]][1],fixed=TRUE)){			
	strsplit(dg[[i]][[x]][1],"$",fixed=TRUE)
	paste(dglistnames[i],"_",strsplit(dg[[i]][[x]][1],"$",fixed=TRUE)[[1]][2],sep="")
	#gsub(grep("$",dglist[i],fixed=TRUE,value=TRUE),replacement=paste(dglistnames[i],"_",sep=""),dg[[i]][[x]][1],fixed=TRUE)
	}else{
	gsub(dg[[i]][[x]][1],replacement=dglistnames[i],dg[[i]][[x]][1],fixed=TRUE)
	}})
	for(j in 1:length(dg[[i]])){
	dg[[i]][[j]] <- dg[[i]][[j]][-1]
	# returns string w/o leading or trailing whitespace:
	dg[[i]][[j]] <- gsub("^\\s+|\\s+$", "", dg[[i]][[j]])
}}

#Extrawurst fuer Regression variables Teil 2:
if(length(regvar)!=0 &&regvar!="none"){  
	reglist <- lapply(1:length(regvar),function(x)grep(paste(regvar[x],"$",sep=""),grep(regvar[x],udg,value=TRUE),fixed=TRUE,value=TRUE))
	reglist <- lapply(reglist,function(x)strsplit(x,"$",fixed=TRUE))
	if(length(which(sapply(reglist,function(x)!length(x)>0)))!=0){
	reglist <- reglist[-(which(sapply(reglist,function(x)!length(x)>0)))]}}

if(length(reglist)!=0){
	reglistnames <- vector()
	for(i in 1:length(reglist)){
		regsublistnames<-vector()
		inregvar<-which(sapply(1:length(reglist[[i]]),function(x)!reglist[[i]][[x]][1]%in%regvar))
		if(length(inregvar)!=0){
			reglist[[i]] <- reglist[[i]][-(inregvar)]
		}
		reglistnames[i] <- reglist[[i]][[1]][1]
		reglist[[i]]<-lapply(1:length(reglist[[i]]),function(x){
					reglist[[i]][[x]]<-reglist[[i]][[x]][-1]
					strsplit(reglist[[i]][[x]],": ")})
		regsublistnames <-lapply(1:length(reglist[[i]]),function(x){
					regsublistnames <- reglist[[i]][[x]][[1]][1]
					if(any(grepl("AO",regsublistnames))){
						regsublistnames<-paste("outlier_",grep("AO",regsublistnames,value=TRUE),sep="")}
					if("Leap Year"%in%regsublistnames){
						regsublistnames<-gsub("Leap Year",replacement="leapyear",regsublistnames,fixed=TRUE)  
					}
					if("Trading Day"%in%regsublistnames){
						regsublistnames<-gsub("Trading Day",replacement="td",regsublistnames,fixed=TRUE)  
					}
					regsublistnames<-regsublistnames
					})
		
		reglist[[i]]<-lapply(1:length(reglist[[i]]),function(x){reglist[[i]][[x]][[1]]<-reglist[[i]][[x]][[1]][-1]
					reglist[[i]][[x]][[1]] <- gsub("^\\s+|\\s+$", "", reglist[[i]][[x]][[1]])})

		if("Leap Year"%in%reglistnames){ 
			reglistnames<-gsub("Leap Year",replacement="leapyear",reglistnames,fixed=TRUE) 
		}
		if("Trading Day"%in%reglistnames){
			reglistnames<-gsub("Trading Day",replacement="td",reglistnames,fixed=TRUE) 
		}
		if(any(grepl("AO",reglistnames))){
		reglistnames[i]<-"outlier"}	
	
		names(reglist[[i]])<-regsublistnames
	}
	dg[(length(dg)+1):(length(dg)+length(reglist))]<-reglist
	names(dg)<-dglistnames<-c(dglistnames,reglistnames)
}else{
	names(dg)<- dglistnames
	reglistnames <- NULL
}
#End Extrawurst Teil 2  

#Checking for derived parameter estimates:
if(any(grepl("nxregderived:",udg,fixed=TRUE))){
	regderived<-grep("nxregderived:",udg,value=TRUE,fixed=TRUE)	
	indnrd<-which(udg==regderived)
	nrd<-as.numeric(strsplit(regderived,": ",fixed=TRUE)[[1]][2])
	regder<-udg[(indnrd+1):(indnrd+nrd)]
	derived.coef<-sapply(strsplit(gsub("$",replacement="_",regder,fixed=TRUE),": ",fixed=TRUE),function(x)
				gsub("\\s+",replacement="",x[[1]]))
}

}
#End x11Regression Option

#Extrawurst fuer Critical Value und die Spektren
for(i in grone.nodoll){
		names(dg[[i]])<-lapply(1:length(dg[[i]]),function(x){names <- dg[[i]][[x]][1]})
for(j in 1:length(dg[[i]])){
		dg[[i]][[j]] <- dg[[i]][[j]][-1]
		dg[[i]][[j]] <- gsub("^\\s+|\\s+$", "", dg[[i]][[j]])
	}
}

#Numerische Daten auch als solche ausgeben lassen:
suppressWarnings(for(i in which(names(dg) %in% numvariables)){
		for(j in 1:length(dg[[i]])){
		num.possible <- as.numeric(dg[[i]][[j]])
	if(length(which(is.na(num.possible)))==0){
	dg[[i]][[j]] <- num.possible	
	}else{	
		num.split <- strsplit(dg[[i]][[j]],"\\s+")
		if(class(num.split)=="list"){
			ex.num <- as.numeric(unlist(num.split))
			num.split <- as.list(num.split[[1]])
		}else{
			ex.num <- as.numeric(num.split)
		}
			if(length(which(!is.na(ex.num)))>0){
			dg[[i]][[j]]<- as.list(num.split)
			dg[[i]][[j]][[which(!is.na(ex.num))]]<- ex.num[which(!is.na(ex.num))]
			}	
		}
	}})	


sel <- c("userdefined","autooutlier","outlier",reglistnames) #werden extra behandelt

for(i in which(names(dg) %in% sel)[!which(names(dg) %in% sel)%in%empty]){
	for(j in 1:length(dg[[i]])){
#(Regular Expressions as used in R)
#+ :The preceding item will be matched one or more times.
#Symbols \d, \s, \D and \S denote the digit and space classes and their negations. 
		dg[[i]][[j]] <- strsplit(dg[[i]][[j]],"\\s+")
		dg[[i]][[j]][[1]]<-as.numeric(dg[[i]][[j]][[1]])
		names(dg[[i]][[j]][[1]])<- c("coef","stderr","tval")#"coef" oder "estimate"
	}}	

if("almostoutlier"%in%names(dg)){
if(!which(names(dg)=="almostoutlier")%in%empty){
	for(j in 1:length(dg[["almostoutlier"]])){
		#cat("j=",j, "\n")
	dg[["almostoutlier"]][[j]] <- strsplit(dg[["almostoutlier"]][[j]],"\\s+")
	dg[["almostoutlier"]][[j]][[1]]<-as.numeric(dg[["almostoutlier"]][[j]][[1]])
	names(dg[["almostoutlier"]][[j]][[1]])<- c("tval_AO","tval_LS","tval_TC")	
}}
}

if(transform=="auto" && dg[["transform"]]!="Automatic selection" &&!x11regress){
dg[["autotransform"]] <- dg[["transform"]] 
names(dg[["autotransform"]]) <-"autotransform"
dg[["transform"]][[1]]<-"Automatic selection"	
names(dg[["transform"]])<-"transform"
}

if(any(grepl("derived.coef",ls()))){
	dg[[length(dg)+1]]<-derived.coef
	names(dg)[[length(dg)]]<-"derived.coef"
}

if(dg[["finalseasonalma"]]!="-"){
dg[["seasonalma"]] <- c(dg[["seasonalma"]],dg[["finalseasonalma"]])
}
dg <- dg[-which(names(dg)=="finalseasonalma")]

if(dg[["finalsamode"]]!="-"){
	dg[["samode"]] <- c(dg[["samode"]],dg[["finalsamode"]])
}
dg <- dg[-which(names(dg)=="finalsamode")]

if(dg[["id.rsdseas"]]=="-")
dg[["id.rsdseas"]]<-"Residual seasonality present"	

if(is.null(outlier) &&!x11regress){
dg[[length(dg)+1]]<-"No outlier detection performed"
names(dg)[[length(dg)]]<-"ifout"
}else if(!is.null(outlier) &&!x11regress){
dg[[length(dg)+1]]<-"Outlier detection performed"
names(dg)[[length(dg)]]<-"ifout"	
}

#filename <- "M:/Meraner/Workspace/Saisonbereinigung_Test/gra/Rout"
#file<-"M:/Meraner/Workspace/Saisonbereinigung_Test/Rout"
#!if exists notwendig
if(file.exists(paste(filename,".","acf",sep=""))){
#sample autocorrelations of residuals
acf <- readLines(con=paste(filename,".","acf",sep=""),n=-1)
#names.acf <- unlist(strsplit(acf[1],"\t"))
names.acf <- c("lag","sample.acf","stderr.acf","Ljung-Box.q","df.q","pval")
acf <- data.frame(do.call(rbind,lapply(strsplit(acf[-(1:2)],"\t"),as.numeric)))
colnames(acf)<-names.acf
dg[[length(dg)+1]]<-acf
names(dg)[[length(dg)]]<-"rsd.acf"
}

if(file.exists(paste(filename,".","pcf",sep=""))){
#sample partial autocorrelations of residuals
pacf <- readLines(con=paste(filename,".","pcf",sep=""),n=-1)
#names.pacf <- unlist(strsplit(pacf[1],"\t"))
names.pacf <- c("lag","sample.pacf","stderr.pacf")
pacf <- data.frame(do.call(rbind,lapply(strsplit(pacf[-(1:2)],"\t"),as.numeric)))
colnames(pacf)<-names.pacf
dg[[length(dg)+1]]<-pacf
names(dg)[[length(dg)]]<-"rsd.pacf"
}

if(file.exists(paste(filename,".","ac2",sep=""))){
#sample autocorrelations of squared residuals
acf2 <-readLines(con=paste(filename,".","ac2",sep=""),n=-1)
names.acf2 <- unlist(strsplit(acf2[1],"\t"))
names.acf2 <- c("lag","sample.acf2","stderr.acf2","Ljung-Box.q","df.q","pval")
acf2 <- data.frame(do.call(rbind,lapply(strsplit(acf2[-(1:2)],"\t"),as.numeric)))
colnames(acf2)<-names.acf2[1:dim(acf2)[2]]#Box Ljung fehlt fuer acf2
#Box.test(,lag=1,type="Ljung-Box",)
dg[[length(dg)+1]]<-acf2
names(dg)[[length(dg)]]<-"rsd.acf2"
}

##Forecasts als data.frame
#if(file.exists(paste(filename,".","fct",sep=""))){
##sample autocorrelations of residuals
#	fct <- readLines(con=paste(filename,".","fct",sep=""),n=-1)
#	names.fct <- unlist(strsplit(fct[1],"\t"))
##	names.fct <- c("..")
#	fct <- data.frame(do.call(rbind,lapply(strsplit(fct[-(1:2)],"\t"),as.numeric)))
#	colnames(fct)<-names.fct
##	dg[[length(dg)+1]]<-fct
##	names(dg)[[length(dg)]]<-"fct"
#out$forecast <- fct
#}
#
##Backcasts als data.frame
#if(file.exists(paste(filename,".","bct",sep=""))){
##sample autocorrelations of residuals
#	bct <- readLines(con=paste(filename,".","bct",sep=""),n=-1)
#	names.bct <- unlist(strsplit(bct[1],"\t"))
##	names.bct <- c("..")
#	bct <- data.frame(do.call(rbind,lapply(strsplit(bct[-(1:2)],"\t"),as.numeric)))
#	colnames(bct)<-names.bct
##	dg[[length(dg)+1]]<-bct
##	names(dg)[[length(dg)]]<-"bct"
#out$backcast <- bct	
#}

#out <- list()
out[["dg"]] <- list()
out[["dg"]] <- dg
  
  out$seats <- seats
  out$file <- file
  out$tblnames <- tblnames
  out$Rtblnames <- Rtblnames
  class(out) <- "x12"
  out  
  
  
}



print.x12 <- function(x,editor=getOption("editor"),...){
  if(!(x$file=="Example_for_X12"))
    filename <- paste(x$file,".out",sep="")
  else
    filename <- paste(paste(searchpaths()[grep("x12",searchpaths())],"/doc/Rout",sep=""),".out",sep="")
  edit(file=filename,editor=editor,...)
}

