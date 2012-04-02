\name{x12Output-class}
\Rdversion{1.1}
\docType{class}
\alias{x12Output-class}
\alias{summary,x12Output-method}

\title{Class \code{"x12Output"}}
\description{
Output class for X12
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("x12Output", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{a1}:}{Object of class \code{"ts"} ~~ }
    \item{\code{d10}:}{Object of class \code{"ts"} ~~ }
    \item{\code{d11}:}{Object of class \code{"ts"} ~~ }
    \item{\code{d12}:}{Object of class \code{"ts"} ~~ }
    \item{\code{d13}:}{Object of class \code{"ts"} ~~ }
    \item{\code{d16}:}{Object of class \code{"ts"} ~~ }
    \item{\code{c17}:}{Object of class \code{"ts"} ~~ }
    \item{\code{d9}:}{Object of class \code{"ts"} ~~ }
    \item{\code{e2}:}{Object of class \code{"ts"} ~~ }
    \item{\code{d8}:}{Object of class \code{"ts"} ~~ }
    \item{\code{b1}:}{Object of class \code{"ts"} ~~ }
    \item{\code{sp0}:}{Object of class \code{"spectrum"} ~~ }
    \item{\code{sp1}:}{Object of class \code{"spectrum"} ~~ }
    \item{\code{sp2}:}{Object of class \code{"spectrum"} ~~ }
    \item{\code{spr}:}{Object of class \code{"spectrum"} ~~ }
    \item{\code{forecast}:}{Object of class \code{"fbcast"} ~~ }
    \item{\code{backcast}:}{Object of class \code{"fbcast"} ~~ }
    \item{\code{dg}:}{Object of class \code{"list"} ~~ }
    \item{\code{seats}:}{Object of class \code{"logical"} ~~ }
    \item{\code{file}:}{Object of class \code{"character"} ~~ }
    \item{\code{tblnames}:}{Object of class \code{"character"} ~~ }
    \item{\code{Rtblnames}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{summary}{\code{signature(object = "x12Output")}: ... }
	 }
}
\author{
Alexander Kowarik
}

\seealso{
\code{\link{X12}},
\code{\linkS4class{x12Single}},
\code{\linkS4class{x12Batch}},
\code{\linkS4class{x12Parameter}},
\code{\linkS4class{x12List}},
\code{\linkS4class{x12Output}},
\code{\linkS4class{x12BaseInfo}},
}
\examples{
data(AirPassengersX12)
summary(AirPassengersX12)
showClass("x12Output")
}
\keyword{asummary}
