\name{warn}
\alias{warn}

\title{List Warnings}
\description{
  Gives a List of Warnings
}
\usage{
warn()
}

\details{
  This function simplyfies the output of \code{\link{warnings}} if there
  are several identical warnings, by counting their occurence
}
\value{
  the table of warnings
}
\author{Werner A. Stahel, ETH Zurich}
\seealso{\code{\link{warnings}}}
\examples{
##  not to be run as an example()
##  for (i in 3:6)
##    m <- matrix(1:7, 3,i)
##  warn()
}

\keyword{utilities}
