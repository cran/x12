\name{summary.x12}
\alias{summary.x12}
\title{Diagnostics summary for objects of class x12}
\description{
  Diagnostics summary for objects of class \code{"x12"}.
}
\usage{
\method{summary}{x12}(object,fullSummary=FALSE, spectra.detail=FALSE,almostout=FALSE,rsd.autocorr=NULL,
                      q2=FALSE,likelihood.stat=FALSE,aape=FALSE,id.rsdseas=FALSE,...)
}
\arguments{
  \item{object}{an object of class \code{"x12"}.}
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
  \item{...}{ignored at the moment}
}
\details{
  Delivers a diagnostics summary.
}
\author{Alexander Kowarik, Angelika Meraner}
\seealso{\code{\link{x12}},
	\code{\link{diagnostics-class}},
	\code{\link{X12-methods}}
}
\examples{
  data(AirPassengers)
\dontrun{
summary(x12(AirPassengers,...),q2=TRUE,res.autocorr="acf")}
}
\keyword{asummary}


