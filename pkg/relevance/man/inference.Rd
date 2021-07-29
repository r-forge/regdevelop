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
inference(estimate = NULL, se = NULL, n = NULL, df = NULL, stcoef=TRUE,
  rlv=TRUE, rlv.threshold=getOption("rlv.threshold"),
  testlevel = getOption("testlevel"), object=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{estimate}{vector of estimates or matrix containing
    estimates and their standard errors. The latter is needed if
    \code{se} is not given --

    ... or a model fit object
  }
  \item{se}{vector of standard errors of the estimates
  }
  \item{n}{number of observations
  }
  \item{df}{degrees of freedom of the residuals
  }
  \item{stcoef}{standardized coefficients, suitable for 
    If \code{NULL}, these will be calculated from \code{object}.
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
  \item{object}{model fit, needed for calculating \code{stcoef}.
  }
  \item{...}{furter arguments, passed to
    \code{termtable} and \code{termeffects}
  }
}
\details{The coefficients divided by standard errors are assumed to be
  t-distributed with \code{df} degrees of freedom.
  For \code{df==Inf}, this is the standard normal distribution.
}
\value{
  A data.frame with the variables
  \item{estimate, se}{coefficients and standard errors}
  \item{ciLow, ciUp}{lower and upper limit of the confidence interval}
  \item{testst}{t-test statistic}
  \item{Sig0}{significance value, i.e., test statistic divided by
    critical value, which in turn is the \code{1-testlevel/2}-quantile
    of the t-distribution.
  }
  \item{p.value}{p value}
  \item{p.symbol}{the conventional symbol corresponding to the p value}
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
}
\author{Werner A. Stahel}

\seealso{
   \code{link{termtable}, link{termeffects}}
}

\examples{
data(d.blast)
rr <-
  lm(log10(tremor)~location+log10(distance)+log10(charge), data=d.blast) 
inference(rr)
}
\keyword{ regression }
\concept{relevance}
