x12 <- function(tso,period=frequency(tso),decimals=2,transform="auto",
		        arima=NULL,sarima=NULL,
				automdl=FALSE,maxorder=c(3,3),maxdiff=c(1,1),
				regvariables=NULL,reguser=NULL,regfile=NULL,regfilestart=NULL,regfileformat=NULL,
                tblnames=NULL,Rtblnames=NULL
                ,addLines=NULL,
                x12path=NULL,x13path=NULL,use="x12",
				seats=FALSE, seatsparameter=NULL,
                sigmalim=c(1.5,2.5),outlier=NULL,outlier_span=NULL,
				file=paste(getwd(),"/Rout",sep=""),forecast_years=NULL,estimate=NULL,slidingspans=NULL,aictest=NULL,onlytd=FALSE
){
  header <- vector()
  header[1] <- "series{"
  header[2] <- 'title="R Output for X12a.exe"'
  header[3] <- paste("decimals=",decimals,sep="")
  header[4] <- paste("start=",paste(start(tso),collapse="."),sep="")
  header[5] <- paste("period=",period,sep="")
  header[6] <- "data=("
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
  addcommands[length(addcommands)+1] <- paste("transform{ function=",transform,"  }",sep="")
  if(!is.null(arima)&&!automdl){
	arima <- paste("(",paste(arima,collapse=","),")",sep="")
    sarima <- paste("(",paste(sarima,collapse=","),")",sep="")
    addcommands[length(addcommands)+1] <- paste("arima{ model=",arima,sarima,"  }",sep="")
  }else if(!is.null(arima)&&automdl)
    cat("Arima and Sarima model specifications are ignored, because automdl is activated! \n")
  if(any(c(!is.null(regvariables),!is.null(reguser),!is.null(regfile)))){
    addcommands[length(addcommands)+1] <- "regression{"
    if(!is.null(regvariables))
      addcommands[length(addcommands)+1] <- paste("variables=(",regvariables,")",sep="")
    if(!is.null(aictest))
      addcommands[length(addcommands)+1] <- paste("aictest=(",aictest,") savelog= aictest",sep="")
    if(!is.null(reguser))
      addcommands[length(addcommands)+1] <- paste("user=(",reguser,")",sep="")
    if(!is.null(regfile))
      addcommands[length(addcommands)+1] <- paste("file='",regfile,"'",sep="")
      addcommands[length(addcommands)+1] <- "}"
  }
  if(!is.null(outlier)){
    addcommands[length(addcommands)+1] <- "outlier {"
    if(outlier=="all")
	  addcommands[length(addcommands)+1] <- "types=(all)"
    else
	  addcommands[length(addcommands)+1] <- paste("types=(",paste(outlier,collapse=""),")",sep="")
    if(!is.null(outlier_span))
      addcommands[length(addcommands)+1] <- paste("span=(",paste(outlier_span,collapse=","),")",sep="")	
	addcommands[length(addcommands)+1] <- "print=(default)"
    addcommands[length(addcommands)+1] <- "}"
  }

  if(!is.null(estimate)){
    addcommands[length(addcommands)+1] <- "estimate {"
    addcommands[length(addcommands)+1] <- "outofsample=yes"
    addcommands[length(addcommands)+1] <- "print=(default + rts) # + rsd"
    addcommands[length(addcommands)+1] <- "savelog=(aic bic afc)"
	addcommands[length(addcommands)+1] <- "}"
    addcommands[length(addcommands)+1] <- "check{"
    addcommands[length(addcommands)+1] <- "print=(default+specresidual+pacfplot)"
    addcommands[length(addcommands)+1] <- "savelog=(nrm lbq)"
    addcommands[length(addcommands)+1] <- "}"
  }
  if(!is.null(slidingspans)){
    addcommands[length(addcommands)+1] <- "slidingspans{" 
    addcommands[length(addcommands)+1] <- "savelog= percent"
    addcommands[length(addcommands)+1] <- "additivesa= percent"
    addcommands[length(addcommands)+1] <- "}" 
  }
    if(automdl){
  addcommands[length(addcommands)+1] <- "automdl {"
  addcommands[length(addcommands)+1] <- "#acceptdefault=yes"
  addcommands[length(addcommands)+1] <- paste("maxorder=(",paste(maxorder,collapse=","),")",sep="")
  addcommands[length(addcommands)+1] <- paste("maxdiff=(",paste(maxdiff,collapse=","),")",sep="")
  addcommands[length(addcommands)+1] <- "balanced=yes"
  addcommands[length(addcommands)+1] <- "savelog=(adf amd b5m mu)"
  addcommands[length(addcommands)+1] <- "}" }
  if(!is.null(forecast_years)){
    addcommands[length(addcommands)+1] <- "forecast {"
	addcommands[length(addcommands)+1] <- paste("maxlead=",forecast_years*12,sep="")
	addcommands[length(addcommands)+1] <- "}"
  }
  if(!seats){
    addcommands[length(addcommands)+1] <- "x11{"
    if(onlytd)
      addcommands[length(addcommands)+1] <- "type= trend"
    addcommands[length(addcommands)+1] <- "sfshort=yes"
    if(!is.null(sigmalim)){
      sigmalim <- paste("(",sigmalim[1],",",sigmalim[2],")",sep="")
      addcommands[length(addcommands)+1] <- paste("sigmalim=",sigmalim,sep="")
    }
    addcommands[length(addcommands)+1] <- "calendarsigma=all"
    addcommands[length(addcommands)+1] <- "excludefcst=yes"
    addcommands[length(addcommands)+1] <- "final=user"
    addcommands[length(addcommands)+1] <- "appendfcst=yes" ###forecast
    addcommands[length(addcommands)+1] <- "savelog=all"
    addcommands[length(addcommands)+1] <- "}" 
  }else{
    addcommands[length(addcommands)+1] <- paste("seats{",seatsparameter,"}",sep="")
  }
  con <- file(paste(file,".spc",sep=""))
  if(!is.null(addLines))
    addcommands <- c(addcommands,addLines)
  writeLines(c(header,datarows,addcommands),con)
  close(con)
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
  writeLines(c(mdcommand,command),con1)
  close(con1)
  system("run.bat")
#  out <- list()
  out <- readx12Out(file,freq_series=frequency(tso),start_series=start(tso),end_series=end(tso),tblnames=tblnames,Rtblnames=Rtblnames)
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
  out
}

readx12Out <- function(file,tblnames=NULL,Rtblnames=NULL,freq_series,start_series,end_series,seats=FALSE){
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
  }else
    filename <- paste(searchpaths()[grep("x12",searchpaths())],"/doc/Rout",sep="")
  for(i in 1:length(tblnames)){
    if(file.exists(paste(filename,".",tblnames[i],sep="")))
      out[[tblnames[i]]] <- ts(read.table(paste(filename,".",tblnames[i],sep=""),header=FALSE,skip=2,sep="	",na.strings="-999")[,2],frequency=freq_series,start=start_series)
  }
  spnames <- c("Spectrum_AdjOri","Spectrum_SA","Spectrum_Irr","Spectrum_Rsd")
  sptblnames <- c("sp0", "sp1", "sp2","spr")
  if(!seats){
    for(i in 1:length(sptblnames)){
      out[[sptblnames[i]]] <- read.table(paste(filename,".",sptblnames[i],sep=""),header=FALSE,skip=2,sep="	")[,2:3]
      names(out[[sptblnames[i]]]) <- c("frequency","spectrum")
    }
  }
  out[["d9"]][out[["d9"]]==-999]<-NA
  out[["Forecast with CI"]] <- list()
  fct <- read.table(paste(filename,".","fct",sep=""),header=FALSE,skip=2,sep="	")
  out[["Forecast with CI"]]$estimate <-ts(fct[,2],frequency=freq_series,start=end_series) 
  out[["Forecast with CI"]]$lower <-ts(fct[,3],frequency=freq_series,start=end_series)
  out[["Forecast with CI"]]$upper <-ts(fct[,4],frequency=freq_series,start=end_series)
  out$seats <- seats
  out$file <- file
  out$tblnames <- tblnames
  out$Rtblnames <- Rtblnames
  class(out) <- "x12"
  out  
  
  
}

plot.x12<-function(x,plots=c(1:9),...){
#plots 1: Original
#plots 2: Original Trend Adjusted
#plots 3: Log Original
#plots 4: Seasonal Factors
#plots 5: Seasonal Factors with SI Ratios
#plots 6: Spectrum Adjusted Orig
#plots 7: Spectrum Seasonal Adjusted
#plots 8: Spectrum Irregular
#plots 9: Spectrum Residulas
  par(ask=TRUE)
  if(x$seats)
    plots <- plots [apply(cbind(!plots==3,!plots==4,!plots==5),1,all)]
  if(any(plots==1)){
    plot_original(x)    
  }
  if(any(plots==2)){
    plot_original_seasonal_trend(x)
  }
  if(any(plots==3)){
    plot_original(x,log_transform=TRUE)
  }
  if(any(plots==4)){
    plot_seasonal_factors(x,SI_Ratios=FALSE)
  }
  if(any(plots==5)){
    plot_seasonal_factors(x)
  }
  if(any(plots==6)){
    plot_spectrum(x,which="original")
  }
  if(any(plots==7)){
    plot_spectrum(x,which="seasonaladj")
  }
  if(any(plots==8)){
    plot_spectrum(x,which="irregular")
  }
  if(any(plots==9)){
    plot_spectrum(x,which="residuals")
  }
  par(ask=FALSE)
}


print.x12 <- function(x,editor="notepad",...){
  con1 <- file("run.bat")
  if(!(x$file=="Example_for_X12"))
    command <- paste(editor," ",x$file,".out",sep="")
  else
    command <- paste(editor," ", paste(searchpaths()[grep("x12",searchpaths())],"/doc/Rout",sep=""),".out",sep="")
  writeLines(command,con1)
  close(con1)
  system("run.bat")
}

plot_seasonal_factors <- function(out,SI_Ratios=TRUE,ylab="Value",xlab="",lwd_seasonal=1,col_seasonal="black",lwd_mean=1,col_mean="blue",col_siratio="darkgreen",col_replaced="red",cex_siratio=.9,cex_replaced=.9,SI_Ratios_replaced=TRUE,plot_legend=TRUE,...){
  if(!SI_Ratios)
    v <- as.vector(out[["d10"]]) # Seasonal Factors
  else
    v <- as.vector(out[["d10"]])[1:length(out[["d8"]])] # Seasonal Factors without forecast
  f <- frequency(out[["d10"]])
  dif <- length(v)%%f
  if(dif>0)
    v[length(v)+(1:(f-dif))]<-NA
  out_matrix <- matrix(v,ncol=f,byrow=TRUE)
  if(f==12){
    lab <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  }else if(f==4){
    lab <- c("Qtr1","Qtr2","Qtr3","Qtr4")
  }else if(f==2){
    lab <- c("1st Half","2nd Half")
  }else{
    lab <- 1:f
  }
  if(SI_Ratios){
    main="Seasonal Factors by period and SI Ratios"
  }else{
    main="Seasonal Factors by period"
  }
  ylim <- c(min(v,na.rm=TRUE)*.95,max(v,na.rm=TRUE)*1.09)
  xlim <- c(0,f)
  plot(1,type="n",main=main,xlim=xlim,ylim=ylim,xaxt="n",ylab=ylab,xlab=xlab,cex=cex_siratio,...)
  axis(1,at=(1:f)-1/2,labels=lab)
  for(i in 0:(f)){    
    abline(v=i,col="grey")
  }
  if(SI_Ratios){
    vv <- as.vector(out[["d8"]]) #final unmodified SI Ratios
    dif <- length(vv)%%f
    if(dif>0)
      vv[length(vv)+(1:(f-dif))]<-NA
    out_matrix2 <- matrix(vv,ncol=f,byrow=TRUE)
    vvv <- as.vector(out[["d9"]]) # final replacement for SI Ratios
    dif <- length(vvv)%%f
    if(dif>0)
      vvv[length(vvv)+(1:(f-dif))]<-NA
    out_matrix3 <- matrix(vvv,ncol=f,byrow=TRUE)
  }
  for(i in 0:(f-1)){
    s <- seq(.1+i,(i+1)-.1,l=nrow(out_matrix))
    m <- mean(out_matrix[,i+1],na.rm=TRUE)
    points(rep(m,2)~c(s[1],s[length(s)]),type="l",col=col_mean,lwd=lwd_mean)
    points(out_matrix[,i+1]~s,type="l",col=col_seasonal,lwd=lwd_seasonal)
    if(SI_Ratios){
      points(out_matrix2[,i+1]~s,pch=20,cex=cex_siratio,col=col_siratio)
      if(SI_Ratios_replaced)
        points(out_matrix3[,i+1]~s,pch=20,cex=cex_replaced,col=col_replaced)
    }
  }
  if(plot_legend){
    if(SI_Ratios){
      if(SI_Ratios_replaced)
        legend(x=(f/2)-1,y=ylim[2],legend=c("Seasonal Factors","Mean","SI Ratio","Replaced SI Ratio"),col=c(col_seasonal,col_mean,col_siratio,col_replaced),pch=c(NA,NA,20,20),lty=c(1,1,NA,NA),bg="white")
      else
        legend(x=(f/2)-1,y=ylim[2],legend=c("Seasonal Factors","Mean","SI Ratio"),col=c(col_seasonal,col_mean,col_siratio),pch=c(NA,NA,20),lty=c(1,1,NA),bg="white")      
    }else
      legend(x=(f/2)-1,y=ylim[2],legend=c("Seasonal Factors","Mean"),col=c(col_seasonal,col_mean),lty=c(1,1),bg="white")
  }
}

plot_original <- function(out,ylab="Value",xlab="Date",main=if(!log_transform){"Original Series"}else{"Logs of the Original Series"},col="black",ytop=1,log_transform=FALSE,...){
  if(!log_transform)
    ts <- out[["a1"]]
  else
    ts <- log(out[["a1"]])
  plot(ts,ylim=c(min(ts,na.rm=TRUE),max(ts,na.rm=TRUE)*ytop),xlab=xlab,ylab=ylab,main=main,col=col,...)
}

plot_original_seasonal_trend <- function(out,ylab="Value",xlab="Date",
  main="Original Series, Seasonally Adjusted Series and Trend",
  col_original="black",col_seasonaladj="blue",col_trend="green",
  lwd_original=1,lwd_seasonaladj=1,lwd_trend=1,
  seasonaladj=TRUE,trend=TRUE,original=TRUE,plot_legend=TRUE,log_transform=FALSE,...){
  if(original)
    plot_original(out,ytop=1.1,col=col_original,main=main,xlab=xlab,ylab=ylab,lwd=lwd_original,log_transform=log_transform,...)
  else
    plot_original(out,ytop=1.1,col=col_original,main=main,xlab=xlab,ylab=ylab,lwd=lwd_original,log_transform=log_transform,type="n",...)
  text_leg <- vector()
  col_leg <- vector()
  if(original){
    text_leg <- "Original"
    col_leg <- c(col_original)
  }
  if(seasonaladj){
    ts_adj <- out[["d11"]]
    if(log_transform)
      ts_adj <- log(ts_adj)
    points(ts_adj,col=col_seasonaladj,type="l",lwd=lwd_seasonaladj)
    text_leg <- c(text_leg,"Seasonally Adjusted")
    col_leg <- c(col_leg,col_seasonaladj)
  }
  if(trend){
    ts_trend <- out[["d12"]]
    if(log_transform)
      ts_trend <- log(ts_trend)
    points(ts_trend,col=col_trend,type="l",lwd=lwd_trend)
    text_leg <- c(text_leg,"Trend")
    col_leg <- c(col_leg,col_trend)
  }
  lty <- rep(1,length(col_leg))
  if(plot_legend){
    if(!log_transform)
      legend(x=start(out[["a1"]])[1],y=max(out[["a1"]]*1.05,na.rm=TRUE)*1.05,lty=lty,legend=text_leg,col=col_leg)
    else
      legend(x=start(out[["a1"]])[1],y=log(max(out[["a1"]]*1.05,na.rm=TRUE))*1.05,lty=lty,legend=text_leg,col=col_leg)
    
  }
}

plot_spectrum <- function(out,which="seasonaladj",xlab="Frequency",ylab="Decibels",
  main="default",
  col_bar="darkgrey",col_seasonal="red",col_td="blue",
  lwd_bar=4,lwd_seasonal=1,lwd_td=1,plot_legend=TRUE)
{
  if(main=="default"){
    if(which=="seasonaladj"){main <- "Spectrum of the Seasonally Adjusted Series"}
    else if(which=="original"){main <- "Spectrum of the Original Series"}        
    else if(which=="irregular"){main <- "Spectrum of the Irregular"}
    else if(which=="residuals"){main <- "Spectrum of the RegARIMA Residuals"}    
  }
  if(which=="seasonaladj")
    which <- "sp1"
  else if(which=="original")
    which <- "sp0"
  else if(which=="irregular")
    which <- "sp2"
  else if(which=="residuals")
    which <- "spr"
 
  plot(out[[which]]$frequency,out[[which]]$spectrum,type="n",xlab=xlab,ylab=ylab,main=main,col=col_bar)
  coord <- par("usr")[3]
  
  for(i in 1:length(out[[which]]$frequency)){
    points(x=rep(out[[which]]$frequency[i],2),y=c(out[[which]]$spectrum[i],coord),type="l",col=col_bar,lwd=lwd_bar)  
  }
  f <- frequency(out[["a1"]])
  abline(v=(1:(f/2))*1/f,col=col_seasonal,lwd=lwd_seasonal)
  if(f==12)
    abline(v=out[[which]]$frequency[c(43,53)],col=col_td,lwd=lwd_td)
  coord<-par("usr")
  if(plot_legend){
    if(f==12)
      legend(coord[1]/1.04,coord[4]*1.02,legend=c("Spectrum","Seasonalpeaks","Trading Day Peaks"),lty=rep(1,3),col=c(col_bar,col_seasonal,col_td),bg="white")
    else
      legend(coord[1]/1.04,coord[4]*1.02,legend=c("Spectrum","Seasonalpeaks"),lty=rep(1,2),col=c(col_bar,col_seasonal),bg="white")
  }
}

