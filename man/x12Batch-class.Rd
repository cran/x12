\name{x12Batch-class}
\Rdversion{1.1}
\docType{class}
\alias{x12Batch-class}
\alias{dim,x12Batch-method}
\alias{length,x12Batch-method}
\alias{summary,x12Batch-method}

\title{Class \code{"x12Batch"}}
\description{
Concatenation of multiple x12Single objects
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("x12Batch", tsList, tsName, x12BaseInfo)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{x12List}:}{Object of class \code{"x12List"} ~~ }
    \item{\code{x12BaseInfo}:}{Object of class \code{"x12BaseInfo"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{cleanHistory}{\code{signature(object = "x12Batch")}: ... }
    \item{dim}{\code{signature(x = "x12Batch")}: ... }
    \item{getP}{\code{signature(object = "x12Batch")}: ... }
    \item{length}{\code{signature(x = "x12Batch")}: ... }
    \item{prev}{\code{signature(object = "x12Batch")}: ... }
    \item{setP}{\code{signature(object = "x12Batch")}: ... }
    \item{summary}{\code{signature(object = "x12Batch")}: ... }
    \item{X12}{\code{signature(object = "x12Batch")}: ... }
	 }
}
\author{
Alexander Kowarik
}


\seealso{
\code{\link{X12}},
\code{\linkS4class{x12Single}},
\code{\linkS4class{x12Batch}},
\code{\linkS4class{x12Parameter}},
\code{\linkS4class{x12List}},
\code{\linkS4class{x12Output}},
\code{\linkS4class{x12BaseInfo}},
\code{\link{summary.x12}},\code{\link{x12}}
}
\examples{
#object containing 4 time series and the corresponding parameters and output
data(AirPassengersX12Batch)
summary(AirPassengersX12Batch)
#summary with oldOutput
summary(AirPassengersX12Batch,oldOutput=10)
#Change the parameter and output of the first series back to the first run
AirPassengersX12Batch <- prev(AirPassengersX12Batch,index=1,n=1)
#summary with oldOutput (--- No valid previous runs. ---)
summary(AirPassengersX12Batch,oldOutput=10)


\dontrun{
#Create new batch object with 4 time series
xb <- new("x12Batch",list(AirPassengers,AirPassengers,AirPassengers,AirPassengers))
# change the automdl to FALSE in all 4 elements
xb <- setP(xb,list(automdl=FALSE))
#change the arima and sarima setting for the first ts object
xb <- setP(xb,list(arima=c(1,1,0),sarima=c(1,1,0)),1)
#change the arima and sarima setting for the second ts object
xb <- setP(xb,list(arima=c(0,1,1),sarima=c(0,1,1)),2)
#change the arima and sarima setting for the third ts object
xb <- setP(xb,list(arima=c(0,1,1),sarima=c(1,1,1)),3)
#change the arima and sarima setting for the fourth ts object
xb <- setP(xb,list(arima=c(1,1,1),sarima=c(1,1,1)),4)
#run X12 on all series
xb <- X12(xb)
summary(xb)
#Set automdl=TRUE for the first ts
xb <- setP(xb,list(automdl=TRUE),1)
#rerun X12 on all series (the binaries will only run on the first one)
xb <- X12(xb)
#summary with oldOutput
summary(xb,oldOutput=10)
#Change the parameter and output of the first series back to the first run
xb <- prev(xb,index=1,n=1)
#summary with oldOutput (--- No valid previous runs. ---)
summary(xb,oldOutput=10)
}
}
\keyword{classes}
