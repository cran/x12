\name{x12Single-class}
\Rdversion{1.1}
\docType{class}
\alias{x12Single-class}
\alias{summary,x12Single-method}

\title{Class \code{"x12Single"}}
\description{
Class consisting of all information for X12
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("x12Single", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{ts}:}{Object of class \code{"ts"} ~~ }
    \item{\code{x12Parameter}:}{Object of class \code{"x12Parameter"} ~~ }
    \item{\code{x12Output}:}{Object of class \code{"x12Output"} ~~ }
    \item{\code{x12OldParameter}:}{Object of class \code{"list"} ~~ }
    \item{\code{x12OldOutput}:}{Object of class \code{"list"} ~~ }
    \item{\code{tsName}:}{Object of class \code{"characterOrNULL"} ~~ }
    \item{\code{firstRun}:}{Object of class \code{"logical"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{cleanHistory}{\code{signature(object = "x12Single")}: ... }
    \item{getP}{\code{signature(object = "x12Single")}: ... }
    \item{prev}{\code{signature(object = "x12Single")}: ... }
    \item{setP}{\code{signature(object = "x12Single")}: ... }
    \item{summary}{\code{signature(object = "x12Single")}: ... }
    \item{X12}{\code{signature(object = "x12Single")}: ... }
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
\dontrun{
s <- new("x12Single",ts=AirPassengers,tsName="air")
s <- setP(s,list(estimate=TRUE,regvariables="AO1950.1",outlier="all",critical=list(LS=3.5,TC=2.5)))
s <- X12(s)
}
}
\keyword{classes}
