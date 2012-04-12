\name{X12}
\docType{methods}
\alias{X12}
\alias{X12-methods}
\alias{X12,ts-method}
\alias{X12,x12Batch-method}
\alias{X12,x12Single-method}
\title{ ~~ Methods for Function \code{X12} in Package \pkg{x12} ~~}
\description{
 ~~ Methods for function \code{X12} in package \pkg{x12} ~~
}
\section{Methods}{
\describe{
\item{\code{signature(object = "ts")}}{
%%  ~~describe this method here~~
}
\item{\code{signature(object = "x12Single")}}{
%%  ~~describe this method here~~
}
\item{\code{signature(object = "x12Batch")}}{
%%  ~~describe this method here~~
}
}}

\usage{
X12(object,x12Parameter=new("x12Parameter"),x12BaseInfo=new("x12BaseInfo"),...)
}

\arguments{
\item{object}{object of class \code{\link{ts}}, \code{\link{x12Single-class}} or \code{\link{x12Batch-class}}}
\item{x12Parameter}{object of class \code{\link{x12Parameter}}}
\item{x12BaseInfo}{object of class \code{\link{x12BaseInfo}}}
\item{...}{at the moment only forceRun=FALSE}
}

\section{Value}{
\describe{
\item{}{An S4 object of class \code{\link{x12Output-class}} if \code{object} is of class \code{ts}}
\item{}{An S4 object of class \code{\link{x12Single-class}} if \code{object} is of class \code{x12Single-class}}
\item{}{An S4 object of class \code{\link{x12Batch-class}} if \code{object} is of class \code{x12Batch-class}}
}
}

\source{
  \url{http://www.census.gov/srd/www/x12a/}
}


\author{
Alexander Kowarik, Angelika Meraner}

\seealso{
\code{\link{summary}},
\code{\link{plot}}
}

\examples{
\dontrun{
x12path <- ".../x12a.exe"
xts <- X12(AirPassengers)
summary(xts)
xs <- X12(new("x12Single",ts=AirPassengers))
summary(xs)
xb<-X12(new("x12Batch",list(AirPassengers,AirPassengers,AirPassengers)))
summary(xb)

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
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
