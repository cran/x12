summary.x12 <- function(object,fullSummary=FALSE,spectra.detail=FALSE,almostout=FALSE,rsd.autocorr=NULL,q2=FALSE,likelihood.stat=FALSE,aape=FALSE,id.rsdseas=FALSE,...){
  summaryworkhorse(object$dg,fullSummary=fullSummary,spectra.detail=spectra.detail,almostout=almostout,rsd.autocorr=rsd.autocorr,q2=q2,likelihood.stat=likelihood.stat,aape=aape,id.rsdseas=id.rsdseas) 
}
summaryworkhorse <- function(x,fullSummary=FALSE,spectra.detail=FALSE,almostout=FALSE,rsd.autocorr=NULL,q2=FALSE,likelihood.stat=FALSE,aape=FALSE,id.rsdseas=FALSE){
  #cat("File: \"",x$file,"\"",sep="","\n")	
	if(length(nchar(unlist(x$outlier)))==0)
		x$outlier<-"-"	
	
	if(fullSummary){
	  spectra.detail=TRUE
	  almostout=TRUE
	  rsd.autocorr=c("acf","pacf","acf2")
	  q2=TRUE
	  likelihood.stat=TRUE
	  aape=TRUE
	  id.rsdseas=TRUE  
 	}
	if(x$x11regress=="no"){	
    cat("\n\tModel Definition\n\n")
    if(x$automdl!="-"){
      cat("ARIMA Model:",unlist(x$arimamdl),"(Automatic Model Choice)\n")	
    }else{
      cat("ARIMA Model:",unlist(x$arimamdl),"\n")#automdl erwaehnen
    }
    #zz <- data.frame("(row names)"= c("aaaaa", "b"), check.names=FALSE)
    if(x$transform=="Automatic selection"){
      cat("Transformation:",unlist(x$transform),":",unlist(x$autotransform),"\n")
    }else{
      cat("Transformation:",unlist(x$transform),"\n")
    }	
    cat("Regression Model:",unlist(x$regmdl),"\n")
    cat("\n\tOutlier Detection\n")
    if(x$ifout=="Outlier detection performed"){
      cat("Critical |t| for outliers:\t\n")
      print(unlist(x$crit))
      cat("Total Number of Outliers:",unlist(x$nout),"\n")
      cat("Automatically Identified Outliers:",unlist(x$nautoout),"\n")
      if(almostout){
        cat("Number of ts values that were almost identified as outliers:",unlist(x$nalmostout),"\n")
      }}	
    else{
      cat("\nNo outlier detection performed\n")	
    }
    rest<-unlist(lapply(strsplit(as.character(x$regmdl),"+",fixed=TRUE),function(x)gsub("^\\s+|\\s+$", "",x)))
    rest<-names(x)[which(names(x)%in%rest)]
    if(almostout){
      liste <- c("outlier","userdefined","leapyear","td","autooutlier",rest,"almostoutlier")
    }else
      liste <- c("outlier","userdefined","leapyear","td",rest,"autooutlier")
    liste<-liste[which(liste%in%names(x))]
    empty <- which(unlist(lapply(1:length(x),function(y)any(x[[y]]=="-"))))
    res <- as.data.frame(do.call(rbind,lapply(which(!liste %in% names(x[empty])),function(j){
                  if(!any(grepl(names(x[liste[j]]),names(x[[liste[j]]])))){
                    names(x[[liste[j]]])<-paste(names(x[liste[j]]),"_",names(x[[liste[j]]]),sep="")	
                  }
                  do.call(rbind,lapply(1:length(x[[liste[j]]]),function(i){
                            c(names(x[[liste[j]]][i]),unlist(x[[liste[j]]][[i]]))}))})))
    if(all(dim(res))>0){
      res[,2:4] <- apply(res[,2:4],2,function(x)as.numeric(formatC(as.numeric(as.character(x)),digits=3,format="f")))
      colnames(res)[1]<-"variable"
      cat("\n\tRegression Model\n")
      print(res)
      if(!is.null(x[["derived.coef"]])){
        cat("* Derived parameter estimates: ",x[["derived.coef"]],"\n")	
      }
      }
	  if(likelihood.stat){
		  cat("\n\tLikelihood Statistics\n")
#		  cat("\n")
#		  lstat<-as.data.frame(matrix(c("Log Likelihood","AIC","AICC","BIC","HQ",x$loglikelihood,x$aic,x$aicc,x$bic,x$hq),ncol=2))
#		  colnames(lstat)<-c("Likelihood Statistics"," ")
#		  print(lstat)
		lstat<-matrix(c("AIC:","AICC:","BIC:","HQ: ","Log Likelihood:",x$aic,x$aicc,x$bic,x$hq,x$loglikelihood),ncol=2)	  
		  write.table(lstat,quote=FALSE,row.names=FALSE, col.names=FALSE,sep="\t")
#write.table(lstat,quote=FALSE,row.names=FALSE, col.names=FALSE)		  
		  
	  }	  
	  if(aape && length(x$aape)>1){
		cat("\nAverage absolute percentage error\n")
		mode<-ifelse(x$aape$aape.mode=="outofsample","out of sample","within sample")
		cat("\tin ",mode," forecasts",sep="","\n")
		aape.mat<-matrix(c("Last year:  ","Last-1 year:","Last-2 year:","Last 3 years:",x$aape$aape.0,x$aape$aape.1,x$aape$aape.2,x$aape$aape.3),ncol=2)	  
		write.table(aape.mat,quote=FALSE,row.names=FALSE, col.names=FALSE,sep="\t")
		}
	cat("\n\tSeasonal Adjustment\n\n")
    cat("Identifiable Seasonality:",unlist(x$id.seas),"\n")
	if(id.rsdseas){
	if(x$id.rsdseas=="none")
	cat("Residual Seasonality: none\n")		
	else
	cat("Residual Seasonality: yes\n")	
	}		
    cat("Seasonal Peaks:",unlist(x$peaks.seas),"\n")
    cat("Trading Day Peaks:",unlist(x$peaks.td),"\n")
    cat("Overall Index of Quality of SA\n(Acceptance Region from 0 to 1)\nQ:",unlist(x$q),"\n")
    if(q2){
      cat("Q2:",unlist(x$q2),"\n")#(Q Statistic computed w/o the M2 Quality Control Statistic)\n")	
    }
    cat("Number of M statistics outside the limits:",unlist(x$nmfail),"\n")
	if(spectra.detail){
      new.names<-function(z){
        for(i in 1:length(x[[z]])){
          if(length(x[[z]][[i]])>1){
            x[[z]][[i]] <- do.call(paste,x[[z]][[i]])	}}
        y<-as.data.frame(x[[z]],row.names="")			
        colnames(y)<-gsub(paste(z,".",sep=""),replacement="",names(unlist(x[[z]])),fixed=TRUE)
        return(y)}
      cat("Spectrum of the original series\n")
      print(new.names("spcori"))	
      cat("Spectrum of the regARIMA model residuals\n")
      print(new.names("spcrsd"))	
      cat("Spectrum of differenced seasonally adjusted series\n")
      print(new.names("spcsa"))
      cat("Spectrum of modified irregular series\n")
      print(new.names("spcirr"))
    }
    #		cat("\n\tSeasonal and Trend Moving Averages\n\n")
    cat("\nSA decomposition:",x$samode[[length(x$samode)]],"\n")		
    if(x$seasonalma[[1]]=="M.S.R."){
      cat("Seasonal moving average used for the final iteration: \n",x$seasonalma[[length(x$seasonalma)]],
          " (Based on the size of the global moving seasonality ratio (msr))\n",sep="")
    }else{
      cat("Moving average used to estimate the seasonal factors:",x$seasonalma[[length(x$seasonalma)]],"\n")	
    }
    cat("Moving average used to estimate the final trend-cycle: ",x$trendma[[length(x$trendma)]],"-term Henderson filter\n",sep="")
    if(!is.null(rsd.autocorr)){
      #rsd.autocorr=c("rsd.acf","rsd.pacf","rsd.acf2")
      if("acf"%in%rsd.autocorr){
        cat("\n\tSample Autocorrelations of the Residuals\n")
        #cat("p-values approximate the probability of observing a q-value at least this
        #large when the model fitted is correct.")
        #!!Diese Schranke (<0.05) kann sich noch aendern falls pickmodel spec implementiert wird  
        cat("(Small p-values (<0.05) indicate model inadequacy (for df.q >0))\n\n")
        sig<-rep("",dim(x$rsd.acf)[1])
        sig[which(x$rsd.acf$pval<0.05 & x$rsd.acf$df.q>0)]<-"*"
        rsd.acf<-cbind(x$rsd.acf,sig)
        colnames(rsd.acf)<-c(colnames(x$rsd.acf),"")
        print(rsd.acf)
      }
      if("pacf"%in%rsd.autocorr){
        cat("\n\tSample Partial Autocorrelations of the Residuals\n\n")
        print(x$rsd.pacf)	
      }
      if("acf2"%in%rsd.autocorr){
        cat("\n\tSample Autocorrelations of the Squared Residuals\n\n")
        print(x$rsd.acf2)
      }
    }
  }else{
    cat("\n\tX11 Regression\n\n")
    cat("Regression Model:",unlist(x$regmdl),"\n")
    cat("\n\tOutlier Detection\n")
    cat("Critical |t| for outliers:",unlist(x$crit),"\t\n")
    cat("Total Number of Outliers:",length(x$out)+length(x$autooutlier)-length(which(x$out=="-"))-length(which(x$autooutlier=="-")),"\n")
    cat("Automatically Identified Outliers:",length(x$autooutlier)-length(which(x$autooutlier=="-")),"\n")
    cat("\n\tRegression Model\n")
    rest<-unlist(lapply(strsplit(as.character(x$regmdl),"+",fixed=TRUE),function(x)gsub("^\\s+|\\s+$", "",x)))
    rest<-names(x)[which(names(x)%in%rest)]
    liste <- c("outlier","userdefined","leapyear","td",rest,"autooutlier")#,"almostoutlier")
	liste<-liste[which(liste%in%names(x))]
	empty <- which(unlist(lapply(1:length(x),function(y)any(x[[y]]=="-"))))
    res <- as.data.frame(do.call(rbind,lapply(which(!liste %in% names(x[empty])),function(j){
                  if(!any(grepl(names(x[liste[j]]),names(x[[liste[j]]])))){
                    names(x[[liste[j]]])<-paste(names(x[liste[j]]),"_",names(x[[liste[j]]]),sep="")	
                  }
                  do.call(rbind,lapply(1:length(x[[liste[j]]]),function(i){
                            c(names(x[[liste[j]]][i]),unlist(x[[liste[j]]][[i]]))}))})))
    res[,2:4] <- apply(res[,2:4],2,function(x)as.numeric(formatC(as.numeric(as.character(x)),digits=3,format="f")))
    colnames(res)[1]<-"variable"
    print(res)
    if(!is.null(x[["derived.coef"]])){
      cat("* Derived parameter estimates: ",x[["derived.coef"]],"\n")	
    }
    cat("\n\tSeasonal Adjustment\n\n")
    cat("Identifiable Seasonality:",unlist(x$id.seas),"\n")
	if(id.rsdseas){
		if(x$id.rsdseas=="none")
			cat("Residual Seasonality: none\n")		
		else
			cat("Residual Seasonality: yes\n")	
	}		
    cat("Seasonal Peaks:",unlist(x$peaks.seas),"\n")
    cat("Trading Day Peaks:",unlist(x$peaks.td),"\n")
    cat("Overall Index of Quality of SA\n(Acceptance region from 0 to 1)\nQ:",unlist(x$q),"\n")
    if(q2){
      cat("Q2:",unlist(x$q2),"(Q statistic computed w/o the M2 quality control statistic)\n")	
    }
    cat("Number of M statistics outside the limits:",unlist(x$nmfail),"\n")
    if(spectra.detail){
      new.names<-function(z){
        for(i in 1:length(x[[z]])){
          if(length(x[[z]][[i]])>1){
            x[[z]][[i]] <- do.call(paste,x[[z]][[i]])	}}
        y<-as.data.frame(x[[z]],row.names="")			
        colnames(y)<-gsub(paste(z,".",sep=""),replacement="",names(unlist(x[[z]])),fixed=TRUE)
        return(y)}
      cat("Spectrum of the original series\n")
      print(new.names("spcori"))	
      cat("Spectrum of differenced seasonally adjusted series\n")
      print(new.names("spcsa"))
      cat("Spectrum of modified irregular series\n")
      print(new.names("spcirr"))
    }
    #		cat("\n\tSeasonal and Trend Moving Averages\n\n")
    cat("\nSA decomposition:",x$samode[[length(x$samode)]],"\n")		
    if(x$seasonalma[[1]]=="M.S.R."){
      cat("Seasonal moving average used for the final iteration: \n",x$seasonalma[[length(x$seasonalma)]],
          " (Based on the size of the global moving seasonality ratio (msr))\n",sep="")
    }else{
      cat("Moving average used to estimate the seasonal factors:",x$seasonalma[[length(x$seasonalma)]],"\n")	
    }
    cat("Moving average used to estimate the final trend-cycle: ",x$trendma[[length(x$trendma)]],"-term Henderson filter\n",sep="")
  }}


