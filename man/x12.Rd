\name{x12}
\alias{x12}
\title{Run x12 on an R TS-object}
\description{
A wrapper function for the x12 binaries. It creates a specification file for an R time series and runs x12,
afterwards the output is read into R.
}
\usage{x12(tso,period=frequency(tso),span=NULL,modelspan=NULL,
		decimals=2,transform="auto",
		arima=NULL,sarima=NULL,
		automdl=FALSE,acceptdefault=FALSE,balanced=TRUE,
		maxorder=c(3,2),maxdiff=c(1,1),
		regvariables=NULL,reguser=NULL,regfile=NULL,usertype=NULL,centeruser=NULL,regfilestart=NULL,#regfileformat=NULL,
		tblnames=NULL,Rtblnames=NULL,addLines=NULL,
		x12path=NULL,x13path=NULL,use="x12",
		seats=FALSE, seatsparameter=NULL,
		sigmalim=c(1.5,2.5),outlier=NULL,critical=NULL,outlier_span=NULL,outlier_method=NULL,
		file="Rout",forecast_years=NULL,backcast_years=NULL,forecast_conf=0.95,estimate=FALSE,
		estOutofsample=TRUE,slidingspans=FALSE,aictest=NULL,
		onlytd=FALSE,sfshort=FALSE,samode=NULL,seasonalma=NULL,trendma=NULL,
		x11appendfcst=TRUE,x11appendbcst=FALSE,x11calendarsigma=NULL,x11excludefcst=TRUE,x11final="user",
		x11regress=FALSE,keep_x12out=FALSE,showWarnings=FALSE)
}
\arguments{
  \item{tso}{a time series object.}
  \item{period}{frequency of the time series.}
  \item{span}{vector of length 4, limiting the data used for the calculations and analysis to a certain time interval.\cr
  Start and end date of said time interval can be specified by 4 integers in the format \code{c(start year, start seasonal period, end year, end seasonal period)}
  If the start or end date of the time series object should be used, the respective year and seasonal period are to be set to \code{NA}.}
  \item{modelspan}{vector of length 4, defining the start and end date of the time interval of the data 
  that should be used to determine all regARIMA model coefficients. Specified in the same way as \code{span}.}
  \item{decimals}{number of decimal places of the data.}
  \item{transform}{transform parameter for x12 (\code{"auto"}, \code{"log"}, \code{"none"}).}
  \item{arima}{vector of length 3, defining the arima parameters.}
  \item{sarima}{vector of length 3, defining the sarima parameters.}
  \item{automdl}{\code{TRUE}/\code{FALSE} for activating auto modeling.}
  \item{acceptdefault}{?}
  \item{balanced}{?}
  \item{maxorder}{maximum order for automdl.}
  \item{maxdiff}{maximum diff. order for automdl.}
  \item{regvariables}{character or character vector representing the names of the regression variables.}
  \item{reguser}{character or character vector defining the user parameters in the regression argument.}
  \item{regfile}{path to the file containing the data values of all \code{reguser} variables.}
  \item{usertype}{character or character vector assigning a type of model-estimated regression effect
  		on each user parameter in the regression argument (\code{"seasonal"}, \code{"td"}, \code{"lpyear"}, \code{"user"}, ...).\cr
  		By specifying a character vector of length greater one each variable can be given its own type.
  		Otherwise the same type will be used for all user parameters.}
  \item{centeruser}{character specifying the removal of the (sample) mean or the seasonal means from 
  		the user parameters in the regression argument (\code{"mean"}, \code{"seasonal"}).\cr
  		Default is no modification of the respective user-defined regressors.}
  \item{regfilestart}{start date for the values of the \code{reguser} variables, specified as a vector of two integers in the format \code{c(year, seasonal period)}.}
  \item{tblnames}{character vector of additional tables to be read into R.}
  \item{Rtblnames}{character vector naming the additional tables.}
  \item{addLines}{%character vector defining additional lines for the spc file.
  list or list of lists 
  in the format \code{list(list("spec1","line","line"),list("spec2","line","line","line"),...)}
  specifying new lines that should be added to the spc file.\cr
  The first element of each list is a character defining the respective spec and the remaining
  elements are characters in x12 syntax defining the lines that should be added, each list element corresponding to one new line
  respectively.}
  \item{x12path}{path to the x12 binaries, for example \code{d:\\x12a\\x12a.exe}.}
  \item{x13path}{path to the x13 binaries, for example \code{d:\\x13\\x13.exe}.}
  \item{use}{\code{"x12"} or \code{"x13"}, at the moment only \code{"x12"} is tested properly.}
  \item{seats}{\code{TRUE}/\code{FALSE} for activating SEATS, at the moment SEATS is not properly implemented.}
  \item{seatsparameter}{string defining the seats parameter.}
  \item{sigmalim}{vector of length 2, defining the limits for sigma in the x11 methodology,
  used to downweight extreme irregular values in the internal seasonal adjustment iterations.}
  \item{outlier}{character or character vector defining the method(s) used for outlier detection (\code{"AO"}, \code{"LS"}, \code{"TC"}, \code{"all"}).}
  \item{critical}{number specifying the critical value used for outlier detection 
  		(same value used for all types of outliers)
  		or named list (possible names of list elements being \code{AO},\code{LS} and \code{TC})
  		where each list element specifies the respective critical value 
  		used for detecting the corresponding type of outlier.\cr
  		If not specified, the default critical value is used.}
  \item{outlier_span}{vector of length 2, defining the span for outlier detection.}
  \item{outlier_method}{character determining how detected outliers should be added to the model (\code{"addone"}, \code{"addall"}).
  If not specified,\code{"addone"} is used by default.}% (For x11 regression this argument cannot be used if the sigma argument is used.)
  \item{file}{path to the output directory and filename, default is the working directory and \code{Rout}.*.}
  \item{forecast_years}{number of years to forecast, default is 1 year.}
  \item{backcast_years}{number of years to backcast, default is no backcasts.}
  \item{forecast_conf}{probability for the confidence interval of forecasts}
  \item{estimate}{if \code{TRUE}, the term "estimate" will be added to the spc file.}
  \item{slidingspans}{if \code{TRUE}, slidingspans will be enabled.}
  \item{estOutofsample}{logical defining whether "out of sample" or "within sample" forecast errors
  should be used in calculating the average magnitude of forecast errors over the last three years.}
  \item{aictest}{character vector defining the regression variables for which an AIC test is to be performed.}
  \item{onlytd}{if \code{TRUE}, x11 will only be used to estimate trend and to adjust according to trading days.}  
  \item{sfshort}{logical controlling the seasonal filter to be used if the series is at most 5 years long.
  If \code{TRUE}, the arguments of the \code{seasonalma} filter will be used wherever possible. 
  If \code{FALSE}, a stable seasonal filter will be used irrespective of \code{seasonalma}.}
  \item{samode}{character defining the type of seasonal adjustment decomposition calculated
  (\code{"mult"}, \code{"add"}, \code{"pseudoadd"}, \code{"logadd"}).}
  \item{seasonalma}{character or character vector of the format \code{c("snxm","snxm", ...)} 
  defining which seasonal nxm moving average(s) should be used for which calendar months or quarters 
  to estimate the seasonal factors.
  If only one ma is specified, the same ma will be used for all months or quarters.
  If not specified, the program will invoke an automatic choice.}
  \item{trendma}{integer defining the type of Henderson moving average used for estimating
  the final trend cycle.
  If not specified, the program will invoke an automatic choice.}
  \item{x11appendfcst}{logical defining whether forecasts should be included in certain x11 tables.}
  \item{x11appendbcst}{logical defining whether forecasts should be included in certain x11 tables.}
  \item{x11calendarsigma}{regulates the way the standard errors used for the detection and adjustment of
  extreme values should be computed (\code{"all"}, \code{"signif"}, \code{"select"} or no specification).}
  \item{x11excludefcst}{logical defining if forecasts and backcasts from the regARIMA model
  should not be used in the generation of extreme values in the seasonal adjustment routines.}
  \item{x11final}{character or character vector specifying which type(s) of prior adjustment factors should be
  removed from the final seasonally adjusted series (\code{"AO"}, \code{"LS"}, \code{"TC"}, \code{"user"}, \code{"none"}).}
  \item{x11regress}{if \code{TRUE}, x11Regression will be performed (using the regression commands above).}
  \item{keep_x12out}{if \code{TRUE}, the output files generated by x12 are stored in the folder "gra" in 
  the output directory and are not deleted at the end of a successful run.}
  \item{showWarnings}{logical defining whether warnings and notes generated by x12 should be returned.
  Errors will be displayed in any case.}
}
\details{
 Generates an x12 specification file, runs x12 and reads the output files. 
}

\value{
\code{x12} returns an object of \link{class} \code{"x12"}.

The function \code{summary} is used to print a summary of the diagnostics results.

An object of class \code{"x12"} is a list containing at least the following components: 

\item{a1}{original time series}
\item{d10}{final seasonal factors}
\item{d11}{final seasonally adjusted data}
\item{d12}{final trend cycle}
\item{d13}{final irregular components}
\item{d16}{combined adjustment factors}
\item{c17}{final weights for irregular component}
\item{d9}{final replacements for SI ratios}
\item{e2}{differenced, transformed, seasonally adjusted data}
\item{d8}{final unmodified SI ratios}
\item{b1}{prior adjusted original series}
\item{forecast}{point forecasts with prediction intervals}
\item{backcast}{point backcasts with prediction intervals}
\item{dg}{a list containing several seasonal adjustment and regARIMA modeling diagnostics, i.e.:\cr
\code{x11regress, transform, samode, seasonalma, trendma, arimamdl, automdl, regmdl, nout, nautoout, 
nalmostout, almostoutlier, crit, outlier, userdefined, autooutlier, peaks.seas, peaks.td, id.seas, 
id.rsdseas, spcrsd, spcori, spcsa, spcirr, q, q2, nmfail, loglikelihood, aic, aicc, bic, hq, aape,
autotransform, ifout, res.acf, res.pacf, res.acf2,...}
}
\item{file}{path to the output directory and filename}
\item{tblnames}{tables read into R}
\item{Rtblnames}{names of tables read into R}
}

\seealso{
  \code{\link{X12}},
  \code{\link{ts}},
  \code{\link{summary.x12}},
  \code{\link{plot.x12}},
  \code{\link{X12-methods}}
}
\examples{
### Examples
data(AirPassengers)
\dontrun{
x12out <- x12(AirPassengers,x12path="d:/x12/x12a.exe",transform="auto",
		arima=c(0,1,1),sarima=c(0,1,1),regvariables="lpyear",
		sigmalim=c(2.0,3.0),outlier="all",critical=list(LS=3.5,TC=3),
		seasonalma="s3x3")
summary(x12out)}	
}
\source{
  \url{http://www.census.gov/srd/www/x12a/}
}
\note{
 Only working with available x12 binaries.
}
\author{Alexander Kowarik, Angelika Meraner}