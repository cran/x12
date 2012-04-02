\name{x12Parameter-class}
\Rdversion{1.1}
\docType{class}
\alias{x12Parameter-class}

\title{Class \code{"x12Parameter"}}
\description{
Parameter class for X12
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("x12Parameter", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    %\item{\code{period}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{span}:}{Object of class \code{"characterOrNULL"} ~~ }
    \item{\code{modelspan}:}{Object of class \code{"characterOrNULL"} ~~ }
    \item{\code{decimals}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{transform}:}{Object of class \code{"character"} ~~ }
    \item{\code{acceptdefault}:}{Object of class \code{"logical"} ~~ }
    \item{\code{balanced}:}{Object of class \code{"logical"} ~~ }
    \item{\code{aictest}:}{Object of class \code{"characterOrNULL"} ~~ }
    \item{\code{arima}:}{Object of class \code{"numericOrNULL"} ~~ }
    \item{\code{sarima}:}{Object of class \code{"numericOrNULL"} ~~ }
    \item{\code{automdl}:}{Object of class \code{"logical"} ~~ }
    \item{\code{maxorder}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{maxdiff}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{regvariables}:}{Object of class \code{"characterOrNULL"} ~~ }
    \item{\code{reguser}:}{Object of class \code{"characterOrNULL"} ~~ }
    \item{\code{regfile}:}{Object of class \code{"characterOrNULL"} ~~ }
    \item{\code{usertype}:}{Object of class \code{"characterOrNULL"} ~~ }
    \item{\code{centeruser}:}{Object of class \code{"characterOrNULL"} ~~ }
    \item{\code{regfilestart}:}{Object of class \code{"characterOrNULL"} ~~ }
    \item{\code{seats}:}{Object of class \code{"logical"} ~~ }
    \item{\code{seatsparameter}:}{Object of class \code{"characterOrNULL"} ~~ }
    \item{\code{sigmalim}:}{Object of class \code{"numericOrNULL"} ~~ }
    \item{\code{outlier}:}{Object of class \code{"characterOrNULL"} ~~ }
    \item{\code{critical}:}{Object of class \code{"listOrNULLOrcharacter"} ~~ }
    \item{\code{outlier_span}:}{Object of class \code{"numericOrNULL"} ~~ }
    \item{\code{outlier_method}:}{Object of class \code{"characterOrNULL"} ~~ }
    \item{\code{forecast_years}:}{Object of class \code{"numericOrNULL"} ~~ }
    \item{\code{backcast_years}:}{Object of class \code{"numericOrNULL"} ~~ }
    \item{\code{forecast_conf}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{estimate}:}{Object of class \code{"logical"} ~~ }
    \item{\code{estOutofsample}:}{Object of class \code{"logical"} ~~ }
    \item{\code{slidingspans}:}{Object of class \code{"logical"} ~~ }
    \item{\code{onlytd}:}{Object of class \code{"logical"} ~~ }
    \item{\code{sfshort}:}{Object of class \code{"logical"} ~~ }
    \item{\code{samode}:}{Object of class \code{"characterOrNULL"} ~~ }
    \item{\code{seasonalma}:}{Object of class \code{"numericOrNULL"} ~~ }
    \item{\code{trendma}:}{Object of class \code{"numericOrNULL"} ~~ }
    \item{\code{x11appendfcst}:}{Object of class \code{"logical"} ~~ }
    \item{\code{x11appendbcst}:}{Object of class \code{"logical"} ~~ }
    \item{\code{x11calendarsigma}:}{Object of class \code{"characterOrNULL"} ~~ }
    \item{\code{x11excludefcst}:}{Object of class \code{"logical"} ~~ }
    \item{\code{x11final}:}{Object of class \code{"character"} ~~ }
    \item{\code{x11regress}:}{Object of class \code{"logical"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{getP}{\code{signature(object = "x12Parameter")}: ... }
    \item{setP}{\code{signature(object = "x12Parameter")}: ... }
	 }
}
\author{
Alexander Kowarik}

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
showClass("x12Parameter")
}
\keyword{classes}
