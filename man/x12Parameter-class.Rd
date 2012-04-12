\name{x12Parameter-class}
\Rdversion{1.1}
\docType{class}
\alias{x12Parameter-class}
\alias{x12Parameter}

\title{Class \code{"x12Parameter"}}
\description{
Parameter class for \code{\link{X12}}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("x12Parameter", ...)}.
%%  ~~ describe objects here ~~ 
}


\section{Slots}{
  \describe{
    %\item{\code{period}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{span}:}{Object of class \code{"characterOrNULL"} - vector of length 4, limiting the data used for the calculations and analysis to a certain time interval.\cr
  Start and end date of said time interval can be specified by 4 integers in the format \code{c(start year, start seasonal period, end year, end seasonal period)}
  If the start or end date of the time series object should be used, the respective year and seasonal period are to be set to \code{NA}.}  
    \item{\code{modelspan}:}{Object of class \code{"characterOrNULL"} - vector of length 4, defining the start and end date of the time interval of the data 
  that should be used to determine all regARIMA model coefficients. Specified in the same way as \code{span}.}
    \item{\code{decimals}:}{Object of class \code{"numeric"} - number of decimal places of the data.}
    \item{\code{transform}:}{Object of class \code{"character"} - transform parameter for x12 (\code{"auto"}, \code{"log"}, \code{"none"}).}
    \item{\code{acceptdefault}:}{Object of class \code{"logical"} - logical for \code{automdl} defining whether the default model should be chosen if the Ljung-Box Q statistic 
  for its model residuals is acceptable.}
    \item{\code{balanced}:}{Object of class \code{"logical"} - logical for \code{automdl} defining whether the automatic model procedure will tend towards balanced
	models. \code{TRUE} yields the same preference as the TRAMO program.}
    \item{\code{aictest}:}{Object of class \code{"characterOrNULL"} - character vector defining the regression variables for which an AIC test is to be performed.}
    \item{\code{arima}:}{Object of class \code{"numericOrNULL"} - vector of length 3, defining the arima parameters.} 
    \item{\code{sarima}:}{Object of class \code{"numericOrNULL"} - vector of length 3, defining the sarima parameters.}
    \item{\code{automdl}:}{Object of class \code{"logical"} - \code{TRUE}/\code{FALSE} for activating auto modeling.}
    \item{\code{maxorder}:}{Object of class \code{"numeric"} - maximum order for \code{automdl}.}
    \item{\code{maxdiff}:}{Object of class \code{"numeric"} - maximum diff. order for \code{automdl}.}
    \item{\code{regvariables}:}{Object of class \code{"characterOrNULL"} - character or character vector representing the names of the regression variables.}
    \item{\code{reguser}:}{Object of class \code{"characterOrNULL"} - character or character vector defining the user parameters in the regression argument.}
    \item{\code{regfile}:}{Object of class \code{"characterOrNULL"} - path to the file containing the data values of all \code{reguser} variables.}
    \item{\code{usertype}:}{Object of class \code{"characterOrNULL"} - character or character vector assigning a type of model-estimated regression effect
  		on each user parameter in the regression argument (\code{"seasonal"}, \code{"td"}, \code{"lpyear"}, \code{"user"}, ...).\cr
  		By specifying a character vector of length greater one each variable can be given its own type.
  		Otherwise the same type will be used for all user parameters.}
    \item{\code{centeruser}:}{Object of class \code{"characterOrNULL"} - character specifying the removal of the (sample) mean or the seasonal means from 
  		the user parameters in the regression argument (\code{"mean"}, \code{"seasonal"}).\cr
  		Default is no modification of the respective user-defined regressors.}
    \item{\code{regfilestart}:}{Object of class \code{"characterOrNULL"} - start date for the values of the \code{reguser} variables, specified as a vector of two integers in the format \code{c(year, seasonal period)}.}
    \item{\code{seats}:}{Object of class \code{"logical"} - \code{TRUE}/\code{FALSE} for activating SEATS, at the moment SEATS is not properly implemented.}
    \item{\code{seatsparameter}:}{Object of class \code{"characterOrNULL"} - string defining the seats parameter.}
    \item{\code{sigmalim}:}{Object of class \code{"numericOrNULL"} - vector of length 2, defining the limits for sigma in the x11 methodology,
  used to downweight extreme irregular values in the internal seasonal adjustment iterations.}
    \item{\code{outlier}:}{Object of class \code{"characterOrNULL"} - character or character vector defining the method(s) used for outlier detection (\code{"AO"}, \code{"LS"}, \code{"TC"}, \code{"all"}).}
    \item{\code{critical}:}{Object of class \code{"listOrNULLOrcharacter"} - number specifying the critical value used for outlier detection 
  		(same value used for all types of outliers)
  		or named list (possible names of list elements being \code{AO},\code{LS} and \code{TC})
  		where each list element specifies the respective critical value 
  		used for detecting the corresponding type of outlier.\cr
  		If not specified, the default critical value is used.}  
    \item{\code{outlier_span}:}{Object of class \code{"numericOrNULL"} - vector of length 2, defining the span for outlier detection.}
    \item{\code{outlier_method}:}{Object of class \code{"characterOrNULL"} - character determining how detected outliers should be added to the model (\code{"addone"}, \code{"addall"}).
  If not specified,\code{"addone"} is used by default.}% (For x11 regression this argument cannot be used if the sigma argument is used.)
    \item{\code{forecast_years}:}{Object of class \code{"numericOrNULL"} - number of years to forecast, default is 1 year.}
    \item{\code{backcast_years}:}{Object of class \code{"numericOrNULL"} - number of years to backcast, default is no backcasts.}
    \item{\code{forecast_conf}:}{Object of class \code{"numeric"} - probability for the confidence interval of forecasts.}
    \item{\code{estimate}:}{Object of class \code{"logical"} - if \code{TRUE}, the term "estimate" will be added to the spc file.}
    \item{\code{estOutofsample}:}{Object of class \code{"logical"} - logical defining whether "out of sample" or "within sample" forecast errors
  should be used in calculating the average magnitude of forecast errors over the last three years.}
    \item{\code{slidingspans}:}{Object of class \code{"logical"} - if \code{TRUE}, slidingspans will be enabled.}
    \item{\code{onlytd}:}{Object of class \code{"logical"} - if \code{TRUE}, x11 will only be used to estimate trend and to adjust according to trading days.}
    \item{\code{sfshort}:}{Object of class \code{"logical"} - }logical controlling the seasonal filter to be used if the series is at most 5 years long.
  If \code{TRUE}, the arguments of the \code{seasonalma} filter will be used wherever possible. 
  If \code{FALSE}, a stable seasonal filter will be used irrespective of \code{seasonalma}.
    \item{\code{samode}:}{Object of class \code{"characterOrNULL"} - character defining the type of seasonal adjustment decomposition calculated
  (\code{"mult"}, \code{"add"}, \code{"pseudoadd"}, \code{"logadd"}).}
    \item{\code{seasonalma}:}{Object of class \code{"numericOrNULL"} - character or character vector of the format \code{c("snxm","snxm", ...)} 
  defining which seasonal nxm moving average(s) should be used for which calendar months or quarters 
  to estimate the seasonal factors.
  If only one ma is specified, the same ma will be used for all months or quarters.
  If not specified, the program will invoke an automatic choice.}
    \item{\code{trendma}:}{Object of class \code{"numericOrNULL"} - integer defining the type of Henderson moving average used for estimating
  the final trend cycle.
  If not specified, the program will invoke an automatic choice.}
    \item{\code{x11appendfcst}:}{Object of class \code{"logical"} - logical defining whether forecasts should be included in certain x11 tables.}
    \item{\code{x11appendbcst}:}{Object of class \code{"logical"} - logical defining whether forecasts should be included in certain x11 tables.}
    \item{\code{x11calendarsigma}:}{Object of class \code{"characterOrNULL"} - regulates the way the standard errors used for the detection and adjustment of
  extreme values should be computed (\code{"all"}, \code{"signif"}, \code{"select"} or no specification).}
    \item{\code{x11excludefcst}:}{Object of class \code{"logical"} - ogical defining if forecasts and backcasts from the regARIMA model
  should not be used in the generation of extreme values in the seasonal adjustment routines.}
    \item{\code{x11final}:}{Object of class \code{"character"} - character or character vector specifying which type(s) of prior adjustment factors should be
  removed from the final seasonally adjusted series (\code{"AO"}, \code{"LS"}, \code{"TC"}, \code{"user"}, \code{"none"}).}
    \item{\code{x11regress}:}{Object of class \code{"logical"} - if \code{TRUE}, x11Regression will be performed (using the regression commands above).}
  }
}
\section{Methods}{
  \describe{
    \item{\code{\link{getP}}}{\code{signature(object = "x12Parameter")}: ... }
    \item{\code{\link{setP}}}{\code{signature(object = "x12Parameter")}: ... }
	 }
}
\author{
Alexander Kowarik, Angelika Meraner}

%\seealso{
%\code{\link{X12}},
%\code{\linkS4class{x12Single}},
%\code{\linkS4class{x12Batch}},
%\code{\linkS4class{x12Parameter}},
%\code{\linkS4class{x12List}},
%\code{\linkS4class{x12Output}},
%\code{\linkS4class{x12BaseInfo}},
%\code{\link{summary.x12}},\code{\link{x12}}
%}
\examples{
showClass("x12Parameter")
}
\keyword{classes}
