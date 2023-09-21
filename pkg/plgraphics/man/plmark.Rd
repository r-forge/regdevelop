\name{plmark}
\alias{plmark}

\title{Labels for Extreme Points
}
\description{
Determine extreme points and get labels for them.
}
\usage{
plmark(x, y = NULL, markextremes = NULL, plabel = NULL, plargs = NULL, ploptions = NULL)
}

\arguments{
  \item{x,y}{coordinates of points. If \code{x} is of length 0,
    it is retrieved from \code{plargs$pldata[,1]}.
  }
  \item{markextremes}{proportion of extreme points to be 'marked'.
    This may be a list of proportions with names
    indicating the variables for which the proportion is to be applied.
    If a vector (of length 2), the elements define the proportions
    for the lower and upper end, respectively.
    In the default case (\code{NULL}), the proportion is obtained from
    \code{ploptions}, which in turn leads to calling the function
    \code{\link{markextremes}} with the argument equal to the number of
    (finite) observations.
  }
  \item{plabel}{character vector of labels to be used for extreme
    points. If \code{NULL}, they are obtained from \code{plargs$plabel}.
  }
  \item{plargs, ploptions}{result of \code{\link{pl.control}}, cf
    \code{\link{plpoints}}
  }
}

\value{A character vector in which the 'marked' observations contain
  the respective label and the others equal \code{""}.
}
\author{Werner A. Stahel
}

\seealso{\code{\link{plyx}}
}
\examples{
  plyx(Sepal.Width ~ Sepal.Length, data=iris)
  ( t.plab <-
    plmark(iris$Sepal.Length, iris$Sepal.Width, markextremes=0.03) )
}
\keyword{ manip }
