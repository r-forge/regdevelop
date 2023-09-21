\name{stdresiduals}
\alias{stdresiduals}
\title{Get Standardized Residuals}
\description{
  Calculates standardized residuals and leverage values.
}
\usage{
stdresiduals(x, residuals=NULL, sigma=x$sigma, weights=NULL,
             leveragelimit = NULL)
}
\arguments{
  \item{x}{a fitted model object}
  \item{residuals}{unstandardized residuals. If missing, they are
    obtained from \code{x}
  }
  \item{sigma}{error standard deviation or other scale}
  \item{weights}{weights}
  \item{leveragelimit}{scalar a little smaller than 1:
    limit on leverage values to avoid unduely large or infinite
    standardized residuals}
}
\value{
  vector or matrix of standardized residuals,
  with attributes\cr
  \code{attr(.,"stdresratio")}:
  ratio of standardized / unstandardized residuals,\cr
  \code{attr(.,"leverage")}: leverage (hat) values,\cr
  \code{attr(.,"weighted")}: weights used in the standardization,\cr
  \code{attr(.,"stddev")}: error standard deviation or scale parameter.
}
\details{
  The difference to \code{\link[MASS]{stdres}()} from package \CRANpkg{MASS}
  is that \code{stdresiduals} also applies to multivariate regression
  and can be used with regression model fits not inheriting from \code{lm}.

  The function uses the \code{\link{qr}} decomposition of \code{object}.
  If necessary, it generates it.
}

\author{Werner A. Stahel, ETH Zurich}

\seealso{\code{\link[MASS]{stdres}; \link{hat}; \link{hatvalues}; \link{influence}}}

\examples{
data(d.blast)
r.blast <-
  lm(log10(tremor)~location+log10(distance)+log10(charge), data=d.blast)
t.stdr <- stdresiduals(r.blast)
showd(t.stdr)
showd(attr(t.stdr, "leverage"))
}
\keyword{regression}

