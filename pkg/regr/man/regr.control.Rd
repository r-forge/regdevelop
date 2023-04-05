\name{regr.control}
\alias{regr.control}
\title{Further Arguments to \code{regr()}}
\description{
  Specify some arguments of minor importance for the function
  \code{\link{regr}()} and set some regroptions.
}
\usage{
regr.control(contrasts = getOption("regr.contrasts"),
  factorNA = getOption("factorNA"), na.action=as.name("nainf.exclude"),
  calcdisp=NULL, suffmean=getOption("suffmean"), dist=NULL,
  model = FALSE, x = TRUE, termtable=TRUE, r2x=getOption("r2x"),
  testlevel = getOption("testlevel"), leveragelimit=NULL, tit=NULL,
  control = NULL)
}
\arguments{
  \item{contrasts}{ . }
  \item{factorNA}{ . }
  \item{na.action}{
}
  \item{calcdisp}{
}
  \item{suffmean}{
%%     ~~Describe \code{suffmean} here~~
}
  \item{dist}{
%%     ~~Describe \code{model} here~~
}
  \item{model}{
%%     ~~Describe \code{model} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{termtable}{
%%     ~~Describe \code{termtable} here~~
}
  \item{r2x}{
%%     ~~Describe \code{vif} here~~
}
  \item{testlevel}{
%%     ~~Describe \code{testlevel} here~~
}
  \item{leveragelimit}{
%%     ~~Describe \code{leveragelim} here~~
}
  \item{tit}{
%%     ~~Describe \code{tit} here~~
}
  \item{control}{parameters to be passed to specific fitting functions.
  }
}
\value{A list, containing the additional arguments needed by \code{\link{regr}()}.
}
\author{
WSt
}

\seealso{
  \code{\link{regr}}
}
\examples{
## This function is not usually called by a user.
}
\keyword{utilities}% use one of  RShowDoc("KEYWORDS")

