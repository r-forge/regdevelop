\name{d.osc15Onesample}
\alias{d.osc15Onesample}
\docType{data}
\title{
Data from the OSC15 replication study, one sample tests
}
\description{
  A small subset of the data of the famous replication study of
  the Open Science Collaboration published in 2015,
  comprising the one sample and paired sample tests,
  used for illustration of the determination of succcess of
  the replications as defined by Stahel (2022) 
}
\usage{data("d.osc15Onesample")}
\format{
  \code{d.osc15}:
  \describe{
    \item{\code{row.names}}{identification number of the study}
    \item{\code{teststatistico, teststatisticr}}{test statistic,
      original paper and replication, respectively} 
    \item{\code{no, nr}}{number of observations, original and replication}
    \item{\code{effecto, effectr}}{effect size as defined by OSC15,
      original and replication }
  }
}
\source{Data repository \code{https://osf.io/jrxtm/}
}
\references{Open Science Collaboration (2015).
  Estimating the reproducibility of psychological science.
  Science 349, 943-952
}
\seealso{\code{\link{d.osc15}}
}
\examples{
data(d.osc15Onesample)

plot(effectr~effecto, data=d.osc15Onesample, xlim=c(0,3.5),ylim=c(0,2.5),
     xaxs="i", yaxs="i")
abline(0,1)

## Compare confidence intervals between original paper and replication
to <- structure(d.osc15Onesample[,c("effecto","teststatistico","no")],
      names=c("effect","teststatistic","n"))
tr <- structure(d.osc15Onesample[,c("effectr","teststatisticr","nr")],
      names=c("effect","teststatistic","n"))
( rr <- replication(to, tr, rlv.threshold=0.1) )
plconfint(rr, refline=c(0,0.1))
plconfint(attr(rr, "estimate"), refline=c(0,0.1))
}
\keyword{datasets}
