\name{x12}
\alias{x12}
\title{Run x12 on a R TS-object}
\description{
A wrapper function for the x12 binaries. It creates an specification file for an R timeseries and runs X12,
afterwards the output is read into R.
}
\usage{x12(tso, period=frequency(tso), decimals=2, transform="auto", arima=NULL, sarima=NULL, automdl=FALSE,
         maxorder=c(3,3), maxdiff=c(1,1), regvariables=NULL, reguser=NULL, regfile=NULL, regfilestart=NULL,
         regfileformat=NULL, tblnames=NULL, Rtblnames=NULL, addLines=NULL, x12path=NULL,
         x13path=NULL, use="x12", seats=FALSE, seatsparameter=NULL, sigmalim=c(1.5,2.5),
         outlier=NULL, outlier_span=NULL, file=paste(getwd(),"/Rout",sep=""), forecast_years=NULL,
         estimate=NULL,slidingspans=NULL,aictest=NULL,onlytd=FALSE)
}
\arguments{
  \item{tso}{Timeseries}
  \item{period}{Frequency of the Timeseries}
  \item{decimals}{Number of decimal places of the data}
  \item{transform}{Transform parameter for x12 ("auto","log","none")}
  \item{arima}{Vector of length 3, defining the arima parameters}
  \item{sarima}{Vector of length 3, defining the sarima parameters}
  \item{automdl}{TRUE/FALSE for activating auto modelling}
  \item{maxorder}{maximum order for automdl}
  \item{maxdiff}{maximum diff. order for automdl}
  \item{regvariables}{Character or character vector representing the names of the reg. variables}
  \item{reguser}{Character defining the user parameter in the regression argument}
  \item{regfile}{Path to the regfile}
  \item{regfilestart}{Start of the variable in the regfile}
  \item{regfileformat}{Format of the regfile}
  \item{tblnames}{character vector of additional tables to be read into R}
  \item{Rtblnames}{character vector naming the additional tables}
  \item{addLines}{character vector defining additional lines for the spc file}
  \item{x12path}{Path to the x12 binaries, for example d:\\x12a\\x12a.exe}
  \item{x13path}{Path to the x13 binaries, for example d:\\x13\\x13.exe}
  \item{use}{"x12" or "x13", at the moment only "x12" is tested properly}
  \item{seats}{TRUE/FALSE fir activating SEATS, at the moment SEATS is not properly implemented}
  \item{seatsparameter}{string defining the seats parameter}
  \item{sigmalim}{vector of length 2, defining the limits for sigma in the x11 methodology}
  \item{outlier}{character vector defining the methods used for outlier detections ("AO","LS","TC")}
  \item{outlier_span}{vector of length 2, defining the span for outlier detection}
  \item{file}{path to the output directory and filename, default is the working directory and Rout.*}
  \item{forecast_years}{Number of years to forecast}
  \item{estimate}{if not NULL, the term "estimate" will be added to the spc file}
  \item{slidingspans}{if not NULL, slidingspans will be enabled}
  \item{aictest}{character vector defining for which regression variables an AIC test is performed}
  \item{onlytd}{if TRUE, x11 will only be used to estimate trend and to adjust according to trading days}  
}
\details{
 Generates a x12 specification file, runs x12 and reads the output files. 
}
\seealso{
  \code{\link{ts}},
  \code{\link[forecast]{forecast}}
  \code{\link{X12GUI}}
}
\examples{
### Examples
}
\source{
  \url{http://www.census.gov/srd/www/x12a/}
}
\note{
 Only working on Windows and with installed x12 binaries.
}
\author{Alexander Kowarik, Statistics Austria,  January 2009}