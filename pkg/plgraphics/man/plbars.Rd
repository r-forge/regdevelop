\name{plbars}
\alias{plbars}

\title{Add bars to a pl plot
}
\description{
  Adds horizontal or vertical bars to a plot
}
\usage{
plbars(x = NULL, y = NULL, midpointwidth = NULL, 
  plargs = NULL, ploptions = NULL, marpar = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x, y}{coordinates for the horizontal and veritical axis,
    respectively. Either of them must have 3 columns.
    If \code{y} has 3 columns, \code{x} must have one only or be
    a vector. Then \code{y[,1]} contains the midpoints, and the other
    two columns determine the endpoints of the bars, which will be
    vertical.
    Analogously if \code{x} has 3 columns.
  }
  \item{midpointwidth}{for \code{plbars}: determines the length of the
    segments that mark the midpoints. See Details.
  }
  \item{plargs, ploptions}{result of \code{\link{pl.control}}, see Details
  }
  \item{marpar}{margin parameters, if already available.
    By default, they will be retieved from \code{ploptions}.
  }
  \item{\dots}{absorbs extra arguments}
}
\value{None.
}
\details{
  For \code{plbars}, the argument \code{midpointwidth} determines the
  length of the segments that mark the midpoint relative to the default,
  which is proportional to the range of the plotting area and inversely
  proportional to the number of (finite) observations.

  \code{plargs} and \code{ploptions} may be specified explicitly.
  Otherwise, they are taken from \code{pl.envir}.
}
  
\author{Werner A. Stahel
}

%% \seealso{
%% }
\examples{
data(d.river)
dd <- plsubset(d.river, 1:2000)
da <- aggregate(dd[,3:7], dd[,"date",drop=FALSE], mean, na.rm=TRUE)
ds <- aggregate(dd[,3:7], dd[,"date",drop=FALSE], sd, na.rm=TRUE)
plyx(O2~date, data=da, type="n")
td <- da$O2 + outer(ds$O2, c(0,-1,1))
plbars(y = td, midpointwidth=0.1, bar.lwd=2)

}


\keyword{aplot}

