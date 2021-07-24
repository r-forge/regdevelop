\name{lassogrp-package}
\alias{lassogrp-package}
\docType{package}
\title{Lasso regression including group lasso and adaptive lasso}
\description{
  lassogrp implements the Lasso regression method for several regression
  types (ordinary, logistic, Poisson and user-provided models).
  It offers the group lasso which is suitable when factors appear in the
  model.
  It also provides a user-friendly way to perform the adaptive lasso,
  which consists of two calls to lasso, the second one using a weighted
  L1 penalization term, where the weights depend on the results of the
  first call.
}
\details{
  The DESCRIPTION file:
  \packageDESCRIPTION{lassogrp}
  \packageIndices{lassogrp}

  Aim: Replaces the \CRANpkg{grplasso} package and extends its functionality.

  The \pkg{lassogrp} package includes methods for printing and plotting as
  well as a cross validation function and functions to extract specified fits
  from the lasso results.

  %% It is in development.
}
\author{
  Werner A. Stahel, ETH Zurich, \email{stahel@stat.math.ethz.ch},
  based on code of Lukas Meier and cosmetics by Martin Maechler.
}
\references{
  Lukas Meier, Sara van de Geer and Peter B\"uhlmann (2008),
  \emph{The Group Lasso for Logistic Regression},
  Journal of the Royal Statistical Society, \bold{70} (1), 53--71
}
\seealso{
  The packages \CRANpkg{lars}, \CRANpkg{glmnet}.
}
\examples{
data(splice)
fit.splice <- lasso(y ~ ., data = splice, model = 'binomial')
}
\keyword{ package }
