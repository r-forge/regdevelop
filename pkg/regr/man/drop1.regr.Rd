\name{drop1.regr}
\title{Drop All Possible Single Terms from or Add Terms to a Model Fitted by \code{regr()}}
\alias{drop1.regr}
\alias{add1.regr}
\alias{drop1.multinom}
\description{
  \code{drop1.regr} / \code{add1.regr} performs a test for the suitability
  of dropping / adding each term in scope from / to a regression model
  fitted by \code{\link{regr}}.
}
\usage{
\method{drop1}{regr}(object, scope = NULL, scale = 0, test = NULL, k = 2,
      sorted = FALSE, add = FALSE, ...)

\method{add1}{regr}(object, scope = NULL, scale = 0, test = NULL, k = 2,
     sorted = FALSE, ...)
}
\arguments{
  \item{object}{a fitted model object}
  \item{scope}{a formula giving the terms to be considered for dropping
    or adding.
    It defaults to the terms in the \code{object}'s formula for \code{drop1}
    and to all first order interactions between these and squared terms
    in the continuous variables for \code{add1}
    (see \code{\link{terms2order}})
  }
  \item{scale}{an estimate of the error variance if applicable. Will be
    recalculated from the fit if ==0 or NULL}
  \item{test}{see \code{\link{drop1}}}
  \item{k}{the penalty constant in AIC / Cp}
  \item{sorted}{if TRUE, the result is sorted according to the AIC or,
    if absent, the p value}
  \item{add}{converts \code{drop1.regr} into an add1 method}
  \item{\dots}{further arguments passed to the model specific methods}
}
\value{
  A data.frame of class \code{"anova"} summarizing the differences in fit
  between the models obtained by dropping or adding the terms and the
  original model given by \code{object}.
}
\author{Werner A. Stahel, ETH Zurich}
\seealso{\code{\link{drop1}}, \code{\link{add1}}}
\examples{
  data(d.blast, package="plgraphics")
  t.r <- regr(log10(tremor)~location+log10(distance)+log10(charge), data=d.blast)
  drop1(t.r)
  add1(t.r)
}
\keyword{regression}
