\name{termtable}
\alias{termtable}
%% \alias{print.termtable}

\title{Statistics for Linear Models, Including Relevance Statistics}
\description{Calculate a table of statistics for (multiple) regression
  mdels with a linear predictor and respective \code{print} method
}
\usage{
termtable(object, summary = NULL, testtype = NULL, r2x = TRUE,
  rlv = TRUE, rlvThres = c(rel = 0.1, coef = 0.1, drop = 0.1, pred = 0.05),
  testlevel = 0.05)

%%\method{print}{termtable}(x, columns = NULL, printstyle = NULL, legend = NULL,
%%  digits = NULL, na.print = "  ", ...)
}

\arguments{
  \item{object}{result of a model fitting function like \code{\link{lm}}
  }
  \item{summary}{result of \code{summary(object)}. If \code{NULL}, the
    \code{summary} will be called.
  }
  \item{testtype}{type of test to be applied for dropping each term in
    turn. If \code{NULL}, it is selected according to the class of the
    object, see Details.
  }
  \item{r2x}{logical: should the collinearity measures "\code{R2.x}"
    for the terms be calculated? 
  }
  \item{rlv}{logical: Should relevances be calculated?
  }
  \item{rlvThres}{Relevance thresholds, vector with the three components
    \cr{\code{rel}:}  {threshold for relative effects,}
    \cr{\code{coef}:}  {for standardized coefficients,}
    \cr{\code{drop}:}  {for drop effects,}
    \cr{\code{pred}:}  {for prediction intervals.}
  }
  \item{testlevel}{1 - confidence level
  }
}
\details{
  If \code{testtype} is not specified, it is determined by the class of
  \code{object} and its attribute \code{family} as follows:
  ...
}
\value{data.frame with the following columns:
  \item{coef}{coefficients for terms with a single degree of freedom}
  \item{df}{degrees of freedom}
  \item{se}{standard error of \code{coef}}
  \item{ciLow, ciUp}{confidence interval for \code{coef}}          
  \item{signif0}{significance value for \code{coef}}
  \item{stcoef}{standardized coefficient (standardized using
    \code{sigma} instead of the response's standard deviation)}
  \item{stciLow, stciUp}{confidence interval for \code{stcoef}}
  \item{testst}{test statistic}
  \item{p.value, p.symbol}{p value and symbol for it}
  \item{R2.x}{collinearity measure
    (= 1 - 1 / vif, wheere vif is the variance inflation factor)}
  \item{coefRle}{estimated relevance of \code{coef}}
  \item{coefRls}{secured relevance, lower end of confidence interval
    for \code{coefRle}}
  \item{coefRlp}{potential relevance, upper end of confidence interval
    for \code{coefRle}}
  \item{dropRle, dropRls, dropRlp}{analogous values for drop effect}
  \item{predRle, predRls, predRlp}{analogous values for prediction effect}
  \item{coefRls.symbol, dropRls.symbol, predRls.symbol}{
    symbols for the secured relevances}
  Attributes:
  {testtype, fitclass, family, dist} {as determined by the
    argument \code{testtype} and the class and attributes of
    \code{object}
  }
}
\references{Werner A. Stahel (2020).
  Measuring Significance and Relevance instead of p-values.
  Submitted
}
\author{Werner A. Stahel
}

\seealso{
  \code{summary(object)$coefficients}
}
\examples{
  data(swiss)
  rr <- lm(Fertility ~ . , data = swiss)
  rt <- termtable(rr)
  rt
  names(rt)
  data.frame(rt)
}
\keyword{regression}
