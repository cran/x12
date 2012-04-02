\name{crossVal}
\docType{methods}
\alias{crossVal}
%\alias{crossValidation}
\alias{crossVal-methods}
\alias{crossVal,ts-method}
\alias{crossVal,x12Single-method}
\alias{crossVal,plotFbcast-methods}

\title{ ~~ Methods for Function \code{crossVal} in Package \pkg{x12} ~~}
\description{
 ~~ Methods for function \code{crossVal} in package \pkg{x12} ~~
}
\section{Methods}{
\describe{

\item{\code{signature(object = "ts")}}{
%%  ~~describe this method here~~
}

%\item{\code{signature(object = "x12Batch")}}{
%%  ~~describe this method here~~
%}

\item{\code{signature(object = "x12Single")}}{
%%  ~~describe this method here~~
}
}}
%\arguments{
%  \item{x}{an object of class \code{"x12Output"} or \code{"x12Single"}.}
%  \item{which}{a string defining the executable of the editor to use.}
%}
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
\examples{
\dontrun{
s <- new("x12Single",ts=AirPassengers,tsName="air")
s <- setP(s,list(estimate=TRUE,regvariables="AO1950.1",outlier="all",critical=list(LS=3.5,TC=2.5),
				backcast_years=1/2,forecast_years=1))

cv<-crossVal(s,showLine=TRUE)
cv
}
}
