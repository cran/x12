#summary.output.x12 <- function(object,fullSummary=FALSE,spectra.detail=FALSE,almostout=FALSE,rsd.autocorr=NULL,q2=FALSE,likelihood.stat=FALSE,aape=FALSE,id.rsdseas=FALSE,...){
#	summaryworkhorse(object$dg,fullSummary=fullSummary,spectra.detail=spectra.detail,almostout=almostout,rsd.autocorr=rsd.autocorr,q2=q2,likelihood.stat=likelihood.stat,aape=aape,id.rsdseas=id.rsdseas) 
#}
summary.output.workhorse <- function(x,fullSummary=FALSE,spectra.detail=FALSE,almostout=FALSE,rsd.autocorr=NULL,q2=FALSE,likelihood.stat=FALSE,aape=FALSE,id.rsdseas=FALSE){
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
	summary.output<-data.frame()
	if(x$x11regress=="no"){			
#		colnames(summary.output)<-c("Diagnostic","Series")
		summary.output[dim(summary.output)[1]+1,1]<-"X11 Regression"
		summary.output[dim(summary.output)[1],2]<-"FALSE"		
		summary.output[dim(summary.output)[1]+1,1]<-"Model Definition"
		if(x$automdl!="-"){
		summary.output[dim(summary.output)[1],2]<-paste("ARIMA Model:",unlist(x$arimamdl),"(Automatic Model Choice)")			
	}else{
		summary.output[dim(summary.output)[1],2]<- paste("ARIMA Model:",unlist(x$arimamdl))		
	}
		
		if(x$transform=="Automatic selection"){
			summary.output[dim(summary.output)[1]+1,1]<-"Transformation"
			summary.output[dim(summary.output)[1],2]<-paste(unlist(x$transform),":",unlist(x$autotransform))
		}else{
			summary.output[dim(summary.output)[1],2]<-paste("Transformation:",unlist(x$transform))
		}	
		
		summary.output[dim(summary.output)[1]+1,1]<-"Regression Model"
		summary.output[dim(summary.output)[1],2]<-paste(unlist(x$regmdl))
#		cat("\n\tOutlier Detection\n")
		if(x$ifout=="Outlier detection performed"){
			summary.output[dim(summary.output)[1]+1,1]<-"Outlier detection performed"
			summary.output[dim(summary.output)[1],2]<-paste("TRUE")
			
#			cat("Critical |t| for outliers:\t\n")
			for(i in 1:length(names(x$crit))){
					summary.output[dim(summary.output)[1]+1,1]<-names(x$crit)[[i]]
					summary.output[dim(summary.output)[1],2]<-paste(x$crit[[i]],collapse=" ")
				}
				summary.output[dim(summary.output)[1]+1,1]<-"Total Number of Outliers"
				summary.output[dim(summary.output)[1],2]<-paste(unlist(x$nout))
				summary.output[dim(summary.output)[1]+1,1]<-"Nr of Automatically Identified Outliers"
				summary.output[dim(summary.output)[1],2]<-paste(unlist(x$nautoout))
			if(almostout){
				summary.output[dim(summary.output)[1]+1,1]<-"Nr of Almost Outliers"
				summary.output[dim(summary.output)[1],2]<-paste(unlist(x$nalmostout))
			}
		}	
		else{
			summary.output[dim(summary.output)[1]+1,1]<-"Outlier detection performed"
			summary.output[dim(summary.output)[1],2]<-paste("FALSE")
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
			res2 <- cbind(paste(1:length(res[,1]),"outlier,coef,stderr,tval"),apply(res,1,paste,collapse=","))
			summary.output<-rbind(summary.output,res2)
			if(!is.null(x[["derived.coef"]])){
				summary.output[dim(summary.output)[1]+1,1]<-"* Derived parameter estimates"
				summary.output[dim(summary.output)[1],2]<-paste(x[["derived.coef"]])
			}
		}
		if(likelihood.stat){
#			cat("\n\tLikelihood Statistics\n")
			lstat<-matrix(c("AIC","AICC","BIC","HQ ","Log Likelihood",x$aic,x$aicc,x$bic,x$hq,x$loglikelihood),ncol=2)	  
			summary.output <- rbind(summary.output,lstat)
		}	  
		
		if(aape && length(x$aape)>1){
#			cat("\nAverage absolute percentage error\n")
			mode<-ifelse(x$aape$aape.mode=="outofsample","out of sample","within sample")
			summary.output[dim(summary.output)[1]+1,1]<-"AAPE mode"
			summary.output[dim(summary.output)[1],2]<-paste(mode)

			aape.mat<-matrix(c("AAPE Last year","AAPE Last-1 year","AAPE Last-2 year","AAPE Last 3 years",x$aape$aape.0,x$aape$aape.1,x$aape$aape.2,x$aape$aape.3),ncol=2)	  
			summary.output <- rbind(summary.output,aape.mat)
		}
#		cat("\n\tSeasonal Adjustment\n\n")
		summary.output[dim(summary.output)[1]+1,1]<-"Identifiable Seasonality"
		summary.output[dim(summary.output)[1],2]<-paste(unlist(x$id.seas))
		if(id.rsdseas){
			summary.output[dim(summary.output)[1]+1,1]<-"Residual Seasonality"
			if(x$id.rsdseas=="none")
				summary.output[dim(summary.output)[1],2]<-"none"
				else
				summary.output[dim(summary.output)[1],2]<-"yes"
			}		
			summary.output[dim(summary.output)[1]+1,1]<-"Seasonal Peaks"
			summary.output[dim(summary.output)[1],2]<-paste(unlist(x$peaks.seas))
			summary.output[dim(summary.output)[1]+1,1]<-"Trading Day Peaks"
			summary.output[dim(summary.output)[1],2]<-paste(unlist(x$peaks.td))
			summary.output[dim(summary.output)[1]+1,1]<-"Q Statistic"
			summary.output[dim(summary.output)[1],2]<-paste(unlist(x$q))
		if(q2){
			summary.output[dim(summary.output)[1]+1,1]<-"Q2 Statistic"
			summary.output[dim(summary.output)[1],2]<-paste(unlist(x$q2))
			}
			summary.output[dim(summary.output)[1]+1,1]<-"Nr of M stats outside limits"
			summary.output[dim(summary.output)[1],2]<-paste(unlist(x$nmfail))
			if(spectra.detail){
			new.names<-function(z){
				for(i in 1:length(x[[z]])){
					if(length(x[[z]][[i]])>1){
						x[[z]][[i]] <- do.call(paste,x[[z]][[i]])	}}
				y<-as.data.frame(x[[z]],row.names="")			
				colnames(y)<-gsub(paste(z,".",sep=""),replacement="",names(unlist(x[[z]])),fixed=TRUE)
				return(y)}
			summary.output[dim(summary.output)[1]+1,1]<-"Spectrum of the original series"
			summary.output[dim(summary.output)[1],2]<-paste(names(new.names("spcori")),":",new.names("spcori"),collapse=",",sep="")
			summary.output[dim(summary.output)[1]+1,1]<-"Spectrum of the regARIMA model residuals"
			summary.output[dim(summary.output)[1],2]<-paste(names(new.names("spcrsd")),":",new.names("spcrsd"),collapse=",",sep="")
			summary.output[dim(summary.output)[1]+1,1]<-"Spectrum of differenced seasonally adjusted series"
			summary.output[dim(summary.output)[1],2]<-paste(names(new.names("spcsa")),":",new.names("spcsa"),collapse=",",sep="")
			summary.output[dim(summary.output)[1]+1,1]<-"Spectrum of modified irregular series"
			summary.output[dim(summary.output)[1],2]<-paste(names(new.names("spcirr")),":",new.names("spcirr"),collapse=",",sep="")
		}
		#		cat("\n\tSeasonal and Trend Moving Averages\n\n")
summary.output[dim(summary.output)[1]+1,1]<-"SA decomposition"
summary.output[dim(summary.output)[1],2]<-paste(x$samode[[length(x$samode)]])

		if(x$seasonalma[[1]]=="M.S.R."){
			summary.output[dim(summary.output)[1]+1,1]<-"Seasonal moving average"
			summary.output[dim(summary.output)[1],2]<-paste(x$seasonalma[[length(x$seasonalma)]],"(Based on msr size)",sep=" ")
			}else{
			summary.output[dim(summary.output)[1]+1,1]<-"Seasonal moving average"
			summary.output[dim(summary.output)[1],2]<-paste(x$seasonalma[[length(x$seasonalma)]])
			}
		summary.output[dim(summary.output)[1]+1,1]<-"Henderson filter"
		summary.output[dim(summary.output)[1],2]<-paste(x$trendma[[length(x$trendma)]],"-term",sep="")
			if(!is.null(rsd.autocorr)){
			#rsd.autocorr=c("rsd.acf","rsd.pacf","rsd.acf2")
			if("acf"%in%rsd.autocorr){
				sig<-rep("",dim(x$rsd.acf)[1])
				sig[which(x$rsd.acf$pval<0.05 & x$rsd.acf$df.q>0)]<-"*"
				rsd.acf<-cbind(x$rsd.acf,sig)
			summary.output<-rbind(summary.output,cbind(rep("acf: lag,sample.acf,stderr.acf,Ljung-Box.q,df.q,pval",dim(rsd.acf)[1]),paste(apply(rsd.acf[,-dim(rsd.acf)[2]],1,paste,collapse=","),rsd.acf[,dim(rsd.acf)[2]])))
			}
			if("pacf"%in%rsd.autocorr){
				summary.output<-rbind(summary.output,cbind(rep("pacf: lag,sample.pacf,stderr.pacf",dim(x$rsd.pacf)[1]),apply(x$rsd.pacf,1,paste,collapse=",")))	
			}
			if("acf2"%in%rsd.autocorr){
				summary.output<-rbind(summary.output,cbind(rep("acf2: lag,sample.acf2,stderr.acf2",dim(x$rsd.acf2)[1]),apply(x$rsd.acf2,1,paste,collapse=",")))	
			}
		}
#		names.sumout<-unique(summary.output[,1])
	}else{
#		cat("\n\tX11 Regression\n\n")
	summary.output[dim(summary.output)[1]+1,1]<-"X11 Regression"
	summary.output[dim(summary.output)[1],2]<-"TRUE"		
	
	summary.output[dim(summary.output)[1]+1,1]<-"Regression Model"
	summary.output[dim(summary.output)[1],2]<-paste(unlist(x$regmdl))
#		cat("\n\tOutlier Detection\n")
#	if(x$ifout=="Outlier detection performed"){
#		summary.output[dim(summary.output)[1]+1,1]<-"Outlier detection performed"
#		summary.output[dim(summary.output)[1],2]<-paste("TRUE")
		
#			cat("Critical |t| for outliers:\t\n")
			summary.output[dim(summary.output)[1]+1,1]<-"aocrit"
			summary.output[dim(summary.output)[1],2]<-paste(unlist(x$crit))
		summary.output[dim(summary.output)[1]+1,1]<-"Total Number of Outliers"
		summary.output[dim(summary.output)[1],2]<-paste(length(x$out)+length(x$autooutlier)-length(which(x$out=="-"))-length(which(x$autooutlier=="-")))
		summary.output[dim(summary.output)[1]+1,1]<-"Nr of Automatically Identified Outliers"
		summary.output[dim(summary.output)[1],2]<-paste(length(x$autooutlier)-length(which(x$autooutlier=="-")))
#	}	
#	else{
#		summary.output[dim(summary.output)[1]+1,1]<-"Outlier detection performed"
#		summary.output[dim(summary.output)[1],2]<-paste("FALSE")
#	}
#		cat("\n\tRegression Model\n")
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
if(all(dim(res))>0){
	res[,2:4] <- apply(res[,2:4],2,function(x)as.numeric(formatC(as.numeric(as.character(x)),digits=3,format="f")))
	colnames(res)[1]<-"variable"
	res2 <- cbind(paste(1:length(res[,1]),"outlier,coef,stderr,tval"),apply(res,1,paste,collapse=","))
	summary.output<-rbind(summary.output,res2)
	if(!is.null(x[["derived.coef"]])){
		summary.output[dim(summary.output)[1]+1,1]<-"* Derived parameter estimates"
		summary.output[dim(summary.output)[1],2]<-paste(x[["derived.coef"]])
	}
}
#		cat("\n\tSeasonal Adjustment\n\n")

summary.output[dim(summary.output)[1]+1,1]<-"Identifiable Seasonality"
summary.output[dim(summary.output)[1],2]<-paste(unlist(x$id.seas))
if(id.rsdseas){
	summary.output[dim(summary.output)[1]+1,1]<-"Residual Seasonality"
	if(x$id.rsdseas=="none")
		summary.output[dim(summary.output)[1],2]<-"none"
	else
		summary.output[dim(summary.output)[1],2]<-"yes"
}		
summary.output[dim(summary.output)[1]+1,1]<-"Seasonal Peaks"
summary.output[dim(summary.output)[1],2]<-paste(unlist(x$peaks.seas))
summary.output[dim(summary.output)[1]+1,1]<-"Trading Day Peaks"
summary.output[dim(summary.output)[1],2]<-paste(unlist(x$peaks.td))
summary.output[dim(summary.output)[1]+1,1]<-"Q Statistic"
summary.output[dim(summary.output)[1],2]<-paste(unlist(x$q))
if(q2){
	summary.output[dim(summary.output)[1]+1,1]<-"Q2 Statistic"
	summary.output[dim(summary.output)[1],2]<-paste(unlist(x$q2))
}
summary.output[dim(summary.output)[1]+1,1]<-"Nr of M stats outside limits"
summary.output[dim(summary.output)[1],2]<-paste(unlist(x$nmfail))


if(spectra.detail){
	new.names<-function(z){
		for(i in 1:length(x[[z]])){
			if(length(x[[z]][[i]])>1){
				x[[z]][[i]] <- do.call(paste,x[[z]][[i]])	}}
		y<-as.data.frame(x[[z]],row.names="")			
		colnames(y)<-gsub(paste(z,".",sep=""),replacement="",names(unlist(x[[z]])),fixed=TRUE)
		return(y)}
	summary.output[dim(summary.output)[1]+1,1]<-"Spectrum of the original series"
	summary.output[dim(summary.output)[1],2]<-paste(names(new.names("spcori")),":",new.names("spcori"),collapse=",",sep="")
	summary.output[dim(summary.output)[1]+1,1]<-"Spectrum of differenced seasonally adjusted series"
	summary.output[dim(summary.output)[1],2]<-paste(names(new.names("spcsa")),":",new.names("spcsa"),collapse=",",sep="")
	summary.output[dim(summary.output)[1]+1,1]<-"Spectrum of modified irregular series"
	summary.output[dim(summary.output)[1],2]<-paste(names(new.names("spcirr")),":",new.names("spcirr"),collapse=",",sep="")
}


#		cat("\n\tSeasonal and Trend Moving Averages\n\n")

summary.output[dim(summary.output)[1]+1,1]<-"SA decomposition"
summary.output[dim(summary.output)[1],2]<-paste(x$samode[[length(x$samode)]])

if(x$seasonalma[[1]]=="M.S.R."){
	summary.output[dim(summary.output)[1]+1,1]<-"Seasonal moving average"
	summary.output[dim(summary.output)[1],2]<-paste(x$seasonalma[[length(x$seasonalma)]],"(Based on msr size)",sep=" ")
}else{
	summary.output[dim(summary.output)[1]+1,1]<-"Seasonal moving average"
	summary.output[dim(summary.output)[1],2]<-paste(x$seasonalma[[length(x$seasonalma)]])
}
summary.output[dim(summary.output)[1]+1,1]<-"Henderson filter"
summary.output[dim(summary.output)[1],2]<-paste(x$trendma[[length(x$trendma)]],"-term",sep="")

}
#		sumout<-unlist(summary.output[,1])
#		names.sumout<-unique(sumout)
#		length.names.sumout<-unlist(lapply(names.sumout,function(x)length(grep(x,sumout))))
		names(summary.output)<-c("DIAGNOSTICS","--- Rout ---")
		spl<-split(summary.output[,1,drop=FALSE],factor(summary.output[,1],levels=unique(summary.output[,1])))		
		ind <- which(sapply(spl, nrow) > 1)		
		v <- lapply(ind, function(x) {data.frame(DIAGNOSTICS=paste(1:nrow(spl[[x]]), names(spl)[x]),stringsAsFactors=FALSE)} )	
		spl[ind] <- v
		new.col<-do.call("rbind",spl)
		summary.output[,1]<-new.col	
		return(summary.output)
#		list(summary.output=summary.output,names.sumout=names.sumout,length.names.sumout=length.names.sumout)
	}


