\name{rlvClass}
\alias{rlvClass}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Relevance Class
}
\description{
  Find the class of relevance on the basis of the confidence interval
  and the relevance threshold
}
\usage{
rlvClass(effect, ci=NULL, relevance=NA)
}

\arguments{
  \item{effect}{either a list of class \code{"inference"}
    (in which case the remaining arguments will be ignored)
    or the estimated effect
  }
  \item{ci}{confidence interval for \code{estimate} or
    width of confidence interval (if of equal length as
    \code{estimate})
  }
  \item{relevance}{relevance threshold
  }
}
\value{Character string: the relevance class, either
  \code{"Rlv"} if the effect is statistically proven to be
  larger than the threshold,
  \code{"Amb"} if the confidence interval contains the threshold,
  \code{"Ngl"} if the interval only covers values
  lower than the threshold, but contains \code{0}, and
  \code{"Ctr"} if the interval only contains negative values.
}
\references{Werner A. Stahel (2020).
  New relevance and significance measures to replace p-values.
  PLOS ONE 16, e0252991, doi: 10.1371/journal.pone.0252991
}
\author{Werner A. Stahel
}

\examples{
  rlvClass(2.3, 1.6, 0.4)  ##  "Rlv"
  rlvClass(2.3, 1.6, 1)  ##  "Sig"
}
\keyword{documentation}
\keyword{misc}
\concept{relevance}
