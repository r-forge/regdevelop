\name{modarg}
\alias{modarg}

\title{
Modify default arguments according to a named vector or list
}
\description{
  Makes it easy to modify one or a few elements of a vector or list
  of default settings.
  This function is to be used within functions that contain vectors
  of control arguments such as colors for different elements of a plot  
}
\usage{
modarg(arg = NULL, default)
}

\arguments{
  \item{arg}{named vector or list of the elements that should override
    the settings in 'default'
  }
  \item{default}{named vector or list of default settings
  }
}

\value{
  Same as the argument 'default' with elements replaced according to
  'arg'.
  See the source code of \code{plmboxes.default} for a typical application.
}
\author{Werner A. Stahel}

\examples{
modarg(c(b="B", c=0), list(a=4, b="bb", c=NA))

df <- ploptions("linewidth")
cbind(df, modarg(c(dot=1.4, dashLongDot=1.3), df))

## These statements lead to a warning:
modarg(c(b=2, d=6), c(a="4", b="bb", c=NA)) 
modarg(1:6, c(a="4", b="bb", c=NA))
}
\keyword{utilities}
