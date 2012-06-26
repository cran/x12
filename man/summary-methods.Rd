\name{summary-methods}
\docType{methods}
\alias{summary-methods}
\alias{summary,x12Output-method}
\alias{summary,x12Single-method}
\alias{summary,x12Batch-method}
\alias{summary}

\title{ ~~ Methods for Function \code{summary} in Package \pkg{x12} ~~}
\description{
Delivers a diagnostics summary for \code{\link{X12}} output.
}
\section{Methods}{
\describe{

%\item{\code{signature(x = "ANY")}}{
%%  ~~describe this method here~~
%}

\item{\code{signature(x = "x12Output")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "x12Single")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(x = "x12Batch")}}{
%%  ~~describe this method here~~
}
}}

\usage{
\S4method{summary}{x12Output}(object, fullSummary=FALSE, spectra.detail=FALSE, almostout=FALSE, rsd.autocorr=NULL,
                      q2=FALSE, likelihood.stat=FALSE, aape=FALSE, id.rsdseas=FALSE,print=TRUE)
\S4method{summary}{x12Single}(object, fullSummary=FALSE, spectra.detail=FALSE, almostout=FALSE, rsd.autocorr=NULL,
                      q2=FALSE, likelihood.stat=FALSE, aape=FALSE, id.rsdseas=FALSE, oldOutput=NULL,print=TRUE)                      
\S4method{summary}{x12Batch}(object, fullSummary=FALSE, spectra.detail=FALSE, almostout=FALSE, rsd.autocorr=NULL,
                      q2=FALSE, likelihood.stat=FALSE, aape=FALSE, id.rsdseas=FALSE, oldOutput=NULL,print=TRUE)                      
}

\arguments{
 \item{object}{object of class \code{\link{x12Output-class}}, \code{\link{x12Single-class}} or \code{\link{x12Batch-class}}.}
  \item{fullSummary}{logical defining whether all available optional diagnostics below should be included in the summary.}
  \item{spectra.detail}{logical defining whether more detail on the spectra should be returned.}
  \item{almostout}{logical defining whether "almost" outliers should be returned.}
  \item{rsd.autocorr}{character or character vector specifying the type of autocorrelation of
  the residuals that should be returned, i.e. the autocorrelations and/or partial autocorrelations 
  of the residuals and/or the autocorrelations of the squared residuals (\code{"acf"}, \code{"pacf"}, \code{"acf2"}).}
  \item{q2}{logical defining whether the second Q statistic, i.e. the Q Statistic computed w/o the M2 Quality Control Statistic,
  should be returned as well.}
  \item{likelihood.stat}{if \code{TRUE}, the likelihood statistics AIC, AICC, BIC and HQ are returned as well
  as the estimated maximum value of the log likelihood function of the model for the untransformed data.}
  \item{aape}{logical defining whether the average absolute percentage error for forecasts should be returned.}
  \item{id.rsdseas}{logical defining whether the presence/absence of residual seasonality should be indicated.}
  \item{oldOutput}{integer specifying the number of previous \code{X12} runs stored in the \code{x12OldOutput} slot of
  an \code{x12Single-class} or an \code{x12Batch-class} object that should be included in the summary.}
  \item{print}{TRUE/FALSE if the summary should be printed.}
}

\author{Alexander Kowarik, Angelika Meraner}

\seealso{\code{\link{prev}},
	\code{\link{cleanHistory}}
}

\examples{

\dontrun{
# Summary of an "x12Single" object
x12path <- ".../x12a.exe"
s <- new("x12Single",ts=AirPassengers,tsName="air")
s <- setP(s,list(estimate=TRUE,regvariables="AO1950.1",outlier="all",critical=list(LS=3.5,TC=2.5),backcast_years=1/2))
s <- X12(s)
summary.output<-summary(s)
s <- X12(setP(s,list(arima=c(0,1,1),sarima=c(0,2,1))))
summary.output<-summary(s,oldOutput=1)
s <- X12(setP(s,list(arima=c(0,1,1),sarima=c(1,0,1))))
summary.output<-summary(s,fullSummary=TRUE,oldOutput=2)

# Summary of an "x12Batch" object
xb <- new("x12Batch",list(AirPassengers,AirPassengers,
				AirPassengers),tsName=c("air1","air2","air3"))
xb <- X12(xb)
xb <- setP(xb,list(arima=c(1,1,0),sarima=c(1,1,0)),1)
xb <- X12(xb)
xb <- setP(xb,list(regvariables=c("AO1955.5","AO1956.1","ao1959.3")),1)
xb <- setP(xb,list(regvariables=c("AO1955.4")),2)
xb<- X12(xb)
xb <- setP(xb,list(outlier="all"))
xb <- setP(xb,list(critical=list(LS=3.5,TC=2.5)),1)
xb <- setP(xb,list(regvariables=c("lpyear")),3)
xb<- X12(xb)
summary.output<-summary(xb,oldOutput=3)
}
}
