\name{smoothRegr}
\alias{smoothRegr}
\alias{smoothLm}

\title{Smoothing function used as a default in plgraphics /
  straight line "smoother"
}
\description{These functions wrap the \code{loess} smoothing function or
  the \code{lm.fit} function in order to meet the argument conventions
  used in the \code{plgraphics} package.\cr
}
\usage{
smoothRegr(x, y, weights = NULL, par = NULL, iterations = 50, minobs=NULL, ...)
smoothLm(x, y, weights = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vector of x values
  }
  \item{y}{vector of y values to be smoothed
  }
  \item{weights}{vector of weigths used for fitting the smooth 
  }
  \item{par}{value for the \code{span} argument of \code{loess}.
  }
  \item{iterations}{number of iterations for the \code{loess} algorithm.
    If \code{==1}, the non-robust, least squares version is applied.
  }
  \item{minobs}{minimal number of observations. If less valid observations
    are provided, the result is \code{NULL}.
  }
  \item{...}{Further arguments, passed to \code{loess}.
  }
}
\value{vector of smoothed values, with an attribute \code{xtrim},
  which is 1 for \code{smoothRegr} and 0 for \code{smoothLm}.
  If \code{loess} fails, NAs will be returned without issuing a
  warning.\cr
}
\author{Werner A. Stahel, ETH Zurich}

\seealso{\code{\link{loess}}, \code{\link{gensmooth}}
}
\examples{
t.x <- (1:50)^1.5
t.y <- log10(t.x) + rnorm(length(t.x),0,0.3)
t.y[40] <- 5
r.sm <- smoothRegr(t.x, t.y, par=0.5)
r.sm1 <- smoothRegr(t.x, t.y, iterations=1, par=0.5)

plot(t.x,t.y)
lines(t.x,r.sm, col=2)
lines(t.x,r.sm1, col=3)

}
\keyword{regression}

