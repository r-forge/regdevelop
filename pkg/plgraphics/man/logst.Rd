\name{logst}
\alias{logst}
\title{Started Logarithmic Transformation}
\description{
Transforms the data by a log10 transformation, modifying small and zero
observations such that the transformation yields finite values.
}
\usage{
logst(data, calib=data, threshold=NULL, mult = 1)
}
\arguments{
  \item{data}{a vector or matrix of data, which is to be transformed}
  \item{calib}{a vector or matrix of data used to calibrate the
    transformation(s),
    i.e., to determine the constant \code{c} needed}
  \item{threshold}{constant c that determines the transformation,
    possibly a vector with a value for each variable.}
  \item{mult}{a tuning constant affecting the transformation of small
    values, see Details}
}
\details{
  Small values are determined by the threshold c. If not given by the
  argument \code{threshold}, then it is determined by the quartiles
  \eqn{q_1}{q1} and \eqn{q_3}{q3} of the non-zero data as those
  smaller than \eqn{c=q_1 / (q_3/q_1)^{mult}}{q1 / (q3/q1)^{mult}}.
  The rationale is that for lognormal data, this constant identifies
  2 percent of the data as small.
  Beyond this limit, the transformation continues linear with the
  derivative of the log curve at this point. See code for the formula.

  The function chooses log10 rather than natural logs because they can
  be backtransformed relatively easily in the mind.

  For more details, see the vignette "logst".
}
\value{
  the transformed data. The value c needed for the transformation is
  returned as \code{attr(.,"threshold")}. 
}
\author{Werner A. Stahel, ETH Zurich}
\note{
  The names of the function alludes to Tudey's idea of "started logs".
}
\examples{
dd <- c(seq(0,1,0.1),5*10^rnorm(100,0,0.2))
dd <- sort(dd)
r.dl <- logst(dd)
attr(r.dl, "threshold")
cbind(data=dd, logst=r.dl, log10=log10(dd))[1:20,]
## logst and log10 coincide for data values
##   larger than the threshold 1.55
plot(dd, r.dl, type="l")
abline(v=attr(r.dl,"threshold"),lty=2)

## precipitation data
data(d.river)
## first 25 days, precipitation data is daily,
##   thus only first record per day
dd <- d.river[d.river$date<as.Date("2010-03-01"),][seq(1,3456,144),]
plmframes(1,3)
plyx(pr~date, data=dd, innerrange=FALSE, main="raw data")
plyx(logst(pr)~date, data=dd, innerrange=FALSE, main="logst data")
abline(h=log10(attr(logst(dd$pr),"threshold")),lty=3,col="blue")
## instead of transforming the data,
##   use a logst scale on the y axis
plyx(pr~date, data=dd, innerrange=FALSE, plscale=c(NA,"logst"),
main="logst scle")

## --------------------------------------------
##  example showing the effect of fixing the threshold
data(d.blast)

##  first 3 locations
dd <- d.blast[as.numeric(d.blast$location)<=3,]
y13 <- logst(dd$tremor)
plyx(y13~as.numeric(dd$location), data=dd,
  xlim=c(0,5), ylim=c(-0.2,NA), xlab="location", ylab="lg(tremor)")

## forth location
tremor4 <- d.blast$tremor[d.blast$location=="loc4"]

## transform tremor of location 4 alone
y4raw <- logst(tremor4)
plpoints(rep(4.3,47), y4raw, col="red")
abline(h=log10(attr(y4raw, "threshold")), lty=3, col="red")
## transform tremor of location 4 using threhold from locations 1-3
y4 <- logst(tremor4, threshold=attr(y13, "threshold"))
plpoints(rep(4,47), y4, col="blue")
abline(h=log10(attr(y13, "threshold")), lty=3, col="blue")
}
\keyword{manip}
