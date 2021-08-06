\name{formatNA}
\alias{formatNA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Print NA values by a Desired Code
}
\description{Recodes the \code{NA} entries in output by a desired code
  like "\code{ .}"
}

\usage{
formatNA(x, na.print = " .", digits = getOption("digits"), ...)
}

\arguments{
  \item{x}{object to be printed, usually a numeric vector or data.frame
  }
  \item{na.print}{code to be used for \code{NA} values
  }
  \item{digits}{number of digits for formatting numeric values
  }
  \item{\dots}{other arguments to \code{format}
  }
}
\details{The \code{na.encode} argument of \code{print} only applies to
  character objects. \code{formatNA} does the same for numeric arguments.
}
\value{Should mimik the value of \code{format}
}
\author{Werner A. Stahel
}

\seealso{\code{\link{format}}
}
\examples{
formatNA(c(1,NA,3))

dd <- data.frame(X=c(1,NA,3), Y=c(4,5, NA), g=factor(c("a",NA,"b")))
(rr <- formatNA(dd, na.print="???"))
str(rr)
}
\keyword{print}
\keyword{manip}
