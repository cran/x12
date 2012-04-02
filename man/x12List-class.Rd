\name{x12List-class}
\Rdversion{1.1}
\docType{class}
\alias{x12List-class}

\title{Class \code{"x12List"}}
\description{
Support class for x12Batch containing multiple x12Single
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("x12List", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{list}"}, from data part.
Class \code{"\linkS4class{vector}"}, by class "list", distance 2.
}
\section{Methods}{
No methods defined with class "x12List" in the signature.
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
showClass("x12List")
}
\keyword{classes}
