\name{correlation}
\alias{correlation}
%% \alias{correlation.default}
%% \alias{correlation.formula}

\title{Correlation with Relevance and Significance Measures}
\description{Inference for a correlation coefficient:
  Collect quantities, including Relevance and Significance measures
}
\usage{
correlation(x, y = NULL, method = c("pearson", "spearman"),
  hypothesis = 0, testlevel=getOption("testlevel"),
  rlv.threshold=getOption("rlv.threshold"), ...)

%% \method{correlation}{default}(x, y = NULL, paired = FALSE, table = NULL, 
%%   hypothesis = 0,var.equal = TRUE,
%%   testlevel=getOption("testlevel"), log = NULL, standardize = NULL, 
%%   rlv.threshold=getOption("rlv.threshold"), ...)
%% \method{correlation}{formula}(x, data = NULL, subset, na.action, log = NULL, ...)
}
\arguments{
  \item{x}{%%a formula or the
    data for the first variable, or matrix or data.frame containing both
    variables 
  }
  \item{y}{data for the second variable
  }
  \item{hypothesis}{the null effect to be tested, and anchor for the
    relevance
  }
  \item{method}{type of correlation, either \code{"pearson"}
    for the ordinary Pearson product moment correlation,
    or \code{"spearman"} for the nonparametric measures
    %%or \code{"kendall"}
  }
  \item{testlevel}{level for the test,
    also determining the confidence level
  }
  \item{rlv.threshold}{Relevance threshold, or a vector of thresholds
    from which the element \code{corr} is taken
  }

%%   For the \code{formula} method:
%% 
%%   \item{formula}{formula of the form  y~x  giving the target y and
%%     condition x variables. For a one-sample situation, use  y~1.
%%   }
%%   \item{data}{data from which the variables are obtained
%%   }
%%   \item{subset, na.action}{subset and na.action to be applied to
%%     \code{data}
%%   }
  \item{...}{further arguments, ignored
  }
}
%% --------------
\value{an object of \code{\link{class}} \code{'inference'}, a
  vector with components
  \describe{
    \item{\code{effect}: }{correlation, transformed with Fisher's z transformation}
    \item{\code{ciLow, ciUp}: }{confidence interval for the effect}
    \item{\code{Rle, Rls, Rlp}: }{relevance measures: estimated, secured, potential}
    \item{\code{Sig0}: }{significance measure for test or 0 effect}
    \item{\code{Sigth}: }{significance measure for test of
      \code{effect}  == relevance threshold}
    \item{\code{p.value}: }{p value for test against 0}
  }
  In addition, it has \code{\link{attributes}}
  \describe{
    \item{\code{method}: }{type of correlation}
    \item{\code{effectname}: }{label for the effect}
    \item{\code{hypothesis}: }{the null effect}
    \item{\code{n}: }{number(s) of observations}
    \item{\code{estimate}: }{estimated correlation}
    \item{\code{conf.int}: }{confidence interval on correlation scale}
    \item{\code{statistic}: }{test statistic}
    \item{\code{data: }}{data.frame containing the two variables}
    \item{\code{rlv.threshold}: }{relevance threshold}
  }
}
%% \details{}    
\references{see those in \code{\link{relevance-package}}.}

\author{Werner A. Stahel}

\seealso{\code{\link{cor.test}}
}
\examples{
correlation(iris[1:50,1:2])
}
\keyword{htest}
\concept{relevance}
