\name{inference}
\alias{inference}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate Confidence Intervals and Relevance and Significance Values
}
\description{Calculates confidence intervals and relevance and
  significance values given estimates, standard errors and, for
  relevance, additional quantities. 
}
\usage{
inference(object = NULL, estimate = NULL, teststatistic = NULL,
  se = NA, n = NULL, df = NULL,
  stcoef = TRUE, rlv = TRUE, rlv.threshold = getOption("rlv.threshold"),
  testlevel = getOption("testlevel"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{A data.frame containing, as its variables,
    the arguments
    \code{estimate} to \code{df}, as far as needed, or
    a vector to be used as \code{estimate} if \code{estimate}
    is not specified...

    ... or a model fit object
  }
  \item{estimate}{estimate(s) of the parameter(s)
  }
  \item{teststatistic}{test statistic(s)
  }
  \item{se}{standard error(s) of the estimate(s)
  }
  \item{n}{number(s) of observations
  }
  \item{df}{degrees of freedom of the residuals
  }
  \item{stcoef}{standardized coefficients.
    If \code{NULL}, these will be calculated from \code{object},
    if the latter is a model fit.
  }
  \item{rlv}{logical: Should relevances be calculated?
  }
  \item{rlv.threshold}{Relevance threshold(s).
    May be a simple number for simple inference, or a
    vector containing the  elements
    \describe{
      \item{\code{stand}:}{threshold for (simple) standardized effects}
      \item{\code{rel}:}{for relative effects,}
      \item{\code{coef}:}{for standardized coefficients,}
      \item{\code{drop}:}{for drop effects,}
      \item{\code{pred}:}{for prediction intervals.}
    }
  }
  \item{testlevel}{1 - confidence level
  }
  \item{...}{furter arguments, passed to
    \code{termtable} and \code{termeffects}
  }
}
\details{The estimates divided by standard errors are assumed to be
  t-distributed with \code{df} degrees of freedom.
  For \code{df==Inf}, this is the standard normal distribution.
}
\value{
  A data.frame of class \code{"inference"}, with the variables
  \item{effect, se}{estimated effect(s), often coefficients,
    and their standard errors}
  \item{ciLow, ciUp}{lower and upper limit of the confidence interval}
  \item{teststatistic}{t-test statistic}
  \item{p.value}{p value}
  \item{Sig0}{significance value, i.e., test statistic divided by
    critical value, which in turn is the \code{1-testlevel/2}-quantile
    of the t-distribution.
  }
  \item{ciLow, ciUp}{confidence interval for \code{effect}}
  \cr
  If \code{rlv} is \code{TRUE},
  \item{stcoef}{standardized coefficient}
  \item{st.Low, st.Up}{confidence interval for \code{stcoef}}
  \item{Rle}{estimated relevance of \code{coef}}
  \item{Rls}{secured relevance, lower end of confidence interval
    for the relevance of \code{coef}}
  \item{Rlp}{potential relevance, upper end of confidence interval
    ...}
  \item{Rls.symbol}{symbols for the secured relevance}
  \item{Rlvclass}{relevance class}
}
\references{Werner A. Stahel (2020).
  New relevance and significance measures to replace p-values.
  PLOS ONE 16, e0252991, doi: 10.1371/journal.pone.0252991
}
\author{Werner A. Stahel}

\seealso{
  \code{link{twosamples}},
  \code{link{termtable}, link{termeffects}}
}

\examples{
data(d.blast)
rr <-
  lm(log10(tremor)~location+log10(distance)+log10(charge),
    data=d.blast) 
inference(rr)
}
\keyword{ regression }
\concept{relevance}
