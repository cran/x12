setMethod("summary",
    signature(object = "x12Output"),
    function (object, fullSummary=FALSE, spectra.detail=FALSE, almostout=FALSE, rsd.autocorr=NULL, q2=FALSE, likelihood.stat=FALSE, aape=FALSE, id.rsdseas=FALSE) 
    {
      if(length(object@dg)>0)
        summaryworkhorse(object@dg,fullSummary=fullSummary,spectra.detail=spectra.detail,almostout=almostout,rsd.autocorr=rsd.autocorr,q2=q2,likelihood.stat=likelihood.stat,aape=aape,id.rsdseas=id.rsdseas)
      else
        cat("You need to run X12 before viewing a summary!\n")
    }
)
setMethod("summary",
    signature(object = "x12Single"),
    function (object, fullSummary=FALSE, spectra.detail=FALSE, almostout=FALSE, rsd.autocorr=NULL, q2=FALSE, likelihood.stat=FALSE, aape=FALSE, id.rsdseas=FALSE,oldOutput=NULL) 
    { 
      if(is.null(oldOutput)){
		if(!is.null(object@tsName)){  
        cat("--------------------------  ",object@tsName,"  ------------------------------------\n")
        cat("-----------------------------------------------------------------------------------\n")
		}else{  
			cat("--------------------------  Rout  ------------------------------------\n")
			cat("-----------------------------------------------------------------------------------\n")
		}
		summary(object@x12Output,fullSummary=fullSummary,spectra.detail=spectra.detail,almostout=almostout,rsd.autocorr=rsd.autocorr,q2=q2,likelihood.stat=likelihood.stat,aape=aape,id.rsdseas=id.rsdseas)
      }else{
        nprev <- min(length(object@x12OldOutput),oldOutput)
		if(!is.null(object@tsName)){  
			cat("--------------------------  ",object@tsName,"  ------------------------------------\n")
			cat("-----------------------------------------------------------------------------------\n")
		}else{  
			cat("--------------------------  Rout  ------------------------------------\n")
			cat("-----------------------------------------------------------------------------------\n")
		}
		summary(object@x12Output,fullSummary=fullSummary,spectra.detail=spectra.detail,almostout=almostout,rsd.autocorr=rsd.autocorr,q2=q2,likelihood.stat=likelihood.stat,aape=aape,id.rsdseas=id.rsdseas)
        if(nprev>0){
          for(i in nprev:1){
            if(i==length(object@x12OldOutput))
              TF <- !identical(object@x12Output,object@x12OldOutput[[i]])
            else if(i!=nprev)
              TF <- !identical(object@x12OldOutput[[i]],object@x12OldOutput[[i+1]])
            else
              TF <- TRUE
            if(TF){
              cat("\n---------------------------  RUN  ",i,"  ----------------------------------------\n")
              summary(object@x12OldOutput[[i]],fullSummary=fullSummary,spectra.detail=spectra.detail,almostout=almostout,rsd.autocorr=rsd.autocorr,q2=q2,likelihood.stat=likelihood.stat,aape=aape,id.rsdseas=id.rsdseas)
            }else{
              cat("--- No valid previous runs. ---\n")              
            }
          }  
        }
      }
    }
)
setMethod("summary",
    signature(object = "x12Batch"),
    function (object, fullSummary=FALSE, spectra.detail=FALSE, almostout=FALSE, rsd.autocorr=NULL, q2=FALSE, likelihood.stat=FALSE, aape=FALSE, id.rsdseas=FALSE,oldOutput=NULL) 
    {
      for(i in 1:length(object@x12List)){
        cat("-----------------------------------------------------------------------------------\n")
        summary(object@x12List[[i]],fullSummary=fullSummary,spectra.detail=spectra.detail,almostout=almostout,rsd.autocorr=rsd.autocorr,q2=q2,likelihood.stat=likelihood.stat,aape=aape,id.rsdseas=id.rsdseas,oldOutput=oldOutput)
      }
    }
)