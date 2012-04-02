\name{prev-methods}
\docType{methods}
\alias{prev}
\alias{prev-methods}
\alias{prev,x12Batch-method}
\alias{prev,x12Single-method}
\alias{cleanHistory}
\alias{cleanHistory-methods}
\alias{cleanHistory,x12Batch-method}
\alias{cleanHistory,x12Single-method}
\title{ ~~ Methods for Function \code{prev} and \code{cleanHistory} in Package \pkg{x12} ~~}
\description{
 ~~ Methods for function \code{prev} in package \pkg{x12} ~~
}
\section{Methods}{
\describe{

\item{\code{signature(object = "x12Batch")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(object = "x12Single")}}{
%%  ~~describe this method here~~
}
}}
\examples{
data(AirPassengersX12)
summary(AirPassengersX12)
# a maximum of 10 previous X12 runs are added to the summary
summary(AirPassengersX12,oldOutput=10)
#the x12Parameter and x12Output of the x12Single is set to the previous run of X12
Ap=prev(AirPassengersX12)
summary(AirPassengersX12,oldOutput=10)
}
\keyword{methods}
\keyword{ ~~ other possible keyword(s) ~~ }
\author{Alexander Kowarik}