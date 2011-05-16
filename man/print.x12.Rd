\name{print.x12}
\alias{print.x12}
\title{Print method for objects of class x12}
\description{
  Print method for objects of class \code{"x12"}.
}
\usage{
\method{print}{x12}(x,editor=getOption("editor"),\dots)
}
\arguments{
  \item{x}{an object of class \code{"x12"}.}
  \item{editor}{a string defining the executable of the editor to use.}
  \item{\dots}{further arguments (for the function edit).}
}
\details{
  Opens the output file of x12 in an external editor
}
\author{Alexander Kowarik}
\seealso{\code{\link{x12}}}
\examples{
  data(AirPassengersX12)
  #print(AirPassengersX12)
}
\keyword{aplot}


