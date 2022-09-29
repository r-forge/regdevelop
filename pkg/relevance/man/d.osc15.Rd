\name{d.osc15}
\alias{d.osc15}
\docType{data}
\title{
Data from the OSC15 replication study
}
\description{
The data of the famous replication study of the Open Science
Collaboration published in 2015
}
\usage{data("d.osc15")}
\format{
  \code{d.osc15}:
  The data frame of OSC15, with 100 observations on 149 variables, of
  which only the most important are described here.
  For a description of all variables, see the repository
  \code{https://osf.io/jrxtm/}
  \describe{
    \item{\code{Study.Num}}{Identification number of the study}
    \item{\code{EffSize.O, EffSize.R}}{effect size as defined by OSC15, original
      paper and replication, respectively}
    \item{\code{Tst.O, Tst.R}}{test statistic, original and replication}
    \item{\code{N.O, N.R}}{number of observations, original and replication}
  }
}
\source{Data repository \code{https://osf.io/jrxtm/}
}
\references{Open Science Collaboration (2015).
  Estimating the reproducibility of psychological science.
  Science 349, 943-952
}
\seealso{\code{\link{d.osc15Onesample}}
}

\examples{
data(d.osc15)

## plot effect sizes of replication against original
## row 9 has an erroneous EffSize.R, and there are 4 missing effect sizes
dd <- na.omit(d.osc15[-9,c("EffSize.O","EffSize.R")]) 
## change sign for negative original effects
dd[dd$EffSize.O<0,] <- -dd[dd$EffSize.O<0,] 
plot(dd)
abline(h=0)
}
\keyword{datasets}
