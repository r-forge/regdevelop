\name{replication}
\alias{replication}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Inference for Replication Studies
}
\description{Calculate inference for a replication study and
  for its comparison with the original
}
\usage{
replication(x, y=NULL,
           testlevel=getOption("testlevel"),
           rlv.threshold=getOption("rlv.threshold") )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{list of class \code{inference}, providing
    the effect estimate (\code{["effect"]}),
    its standard error (\code{["se"]}),
    the number of observations (\code{["n"]}),
    and the scatter (\code{["scatter"]})
    for the 'original' study, or
    a matrix or data.frame containing this information
    as the first row.
  }
  \item{y}{the same, for the replication study;
    if empty or \code{NULL},
    the second row of argument \code{x} is assumed
    to contain the information about the replication.
  }
  \item{testlevel}{level of statistical tests
  }
  \item{rlv.threshold}{threshold of relevance;
    if this is a vector, the first element will be used.
  }
}
\value{
  A list of class \code{inference} and \code{replication}
  containing the results of the comparison between the studies
  and, as an attribute, the results for the replication.
}
\references{Werner A. Stahel (2020).
  Measuring Significance and Relevance instead of p-values.
  Submitted; available in the documentation.
}
\author{Werner A. Stahel
}

\seealso{\code{\link{relevance}}, \code{\link{rplConfint}}
}
\examples{
data(d.osc15Onesample)
tx <- structure(d.osc15Onesample[,c("effecto","teststatistico","no")],
      names=c("effect","teststatistic","n"))
ty <- structure(d.osc15Onesample[,c("effectr","teststatisticr","nr")],
      names=c("effect","teststatistic","n"))
replication(tx, ty, rlv.threshold=0.1)
}
\keyword{statistics}
