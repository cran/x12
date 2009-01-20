\name{plot.x12}
\alias{plot.x12}
\title{Plot method for objects of class x12}
\description{
  Plot method for objects of class \code{"x12"}.
}
\usage{
\method{plot}{x12}(x,plots=c(1:9), \dots)
}
\arguments{
  \item{x}{an object of class \code{"x12"}.}
  \item{plots}{a vector containing numbers between 1 and 9.}
  \item{\dots}{further arguments (currently ignored).}
}
\details{
  Plots:
  1: Original
  2: Original Trend Adjusted
  3: Log Original
  4: Seasonal Factors
  5: Seasonal Factors with SI Ratios
  6: Spectrum Adjusted Original
  7: Spectrum Seasonal Adjusted
  8: Spectrum Irregular
  9: Spectrum Residulas
}
\author{Alexander Kowarik, Statistics Austria,  March 2009}
\seealso{\code{\link{x12}}}
\examples{
  data(AirPassengersX12)
  #plot(AirPassengersX12)
}
\keyword{aplot}


