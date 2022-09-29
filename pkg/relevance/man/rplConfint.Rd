\name{rplConfint}
\alias{rplConfint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Confidence Intervals for Replication Studies
}
\description{Calculate confidence intervals for a rplication study and
  the original study and for their comparison
}
\usage{
rplConfint(rr = list(),
  effecto = rr$effecto, teststatistico = rr$teststatistico,
  no = rr$no, scattero = rr$scattero, seo = rr$seo,
  effectr = rr$effectr, teststatisticr = rr$teststatisticr,
  nr = rr$nr, scatterr = rr$scatterr, ser = rr$ser,
  rlv.threshold = rep(getOption("rlv.threshold")[1],2))
}

\arguments{
  \item{rr}{A data frame containing, if given, the following arguents
  }
  \item{effecto, effectr}{Effect estimate for original and replication,
    respectively}
  \item{teststatistico, teststatisticr}{test statistics}
  \item{no, nr}{numbers of observations}
  \item{scattero, scatterr}{scatters}
  \item{seo, ser}{standard errors}
  \item{rlv.threshold}{relevance threshold
  }
}
\value{A data.frame with the following columns
}
\references{Werner A. Stahel (2020).
  Measuring Significance and Relevance instead of p-values.
  Submitted
}
\author{Werner A. Stahel
}

\seealso{\code{\link{replication}}
}
\examples{
data(d.osc15Onesample)
rplConfint(d.osc15Onesample)
}
\keyword{htest}
