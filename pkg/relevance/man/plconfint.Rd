\name{plconfint}
\alias{plconfint}
\alias{pltwosamples}
\alias{pltwosamples.default}
\alias{pltwosamples.formula}

\title{Plot Confidence Intervals
}
\description{Plot confidence or relevance interval(s)
  for several samples and for the comparison of two samples,
  also useful for replications and original studies
}
\usage{
plconfint(x, y = NULL, select=NULL, overlap = NULL, pos = NULL,
          xlim = NULL, refline = 0, add = FALSE, bty = "L", col = NULL,
          plpars = list(lwd=c(2,3,1,4,2), posdiff=0.35,
                        markheight=c(1, 0.6, 0.6), extend=NA, reflinecol="gray70"),
          label = TRUE, label2 = NULL, xlab="", ...)

pltwosamples(x, ...)
\method{pltwosamples}{default}(x, y = NULL, overlap = TRUE, ...)
\method{pltwosamples}{formula}(formula, data = NULL, ...)
}

\arguments{
  \item{x}{For \code{plconfint}:
    A vector of length >=2 or a matrix
    with this number of columns, containing
    \describe{
      \item{\code{[,1]}: }{the estimate}
      \item{\code{[,2]}: }{if \code{x} is of length 2:
	width of (symmetric) confidence interval}
      \item{\code{[,2:3]}: }{if of length >2: the interval end points}
      \item{\code{[,4:5]}: }{if of length >=5:
	values for additional ticks on the intervals,
	typically indicating the end points of a
	shortened interal, see Details}
    }

    For \code{pltwosamples}:
    A formula or the data for the first sample --
    or a list or matrix or data.frame with two components/columns
    corresponding to the two samples
  }
  \item{y}{data for a second confidence interval (for \code{plconfint}
    or the second sample (for \code{pltwosamples})
  }
  \item{select}{selects samples, effects, or studies}
  \item{overlap}{logical: should shortened intervals be shown to show
    significance of differences?
    see Details}
  \item{pos}{positions of the bars in vertical direction
  }
  \item{xlim}{limits for the horizontal axis. \code{NA}s will be
    replaced by the respective element of the range of the x values.
  }
  \item{refline}{\code{x} values for which vertical reference lines are
    drawn}
  \item{add}{logical: should the plotted elements be added to an
    existing plot?}
  \item{bty}{type of 'box' around the plot, see \code{par}
  }
  \item{col}{color to be used for the confidence intervals,
    usually a vector of colors if used.
  }
  \item{plpars}{graphical options, see Details
  }
  \item{label, label2}{labels for intervals (or intervall pairs)
    to be dislayed on the left and right hand margin, respectivly.
    If \code{label} is \code{TRUE}, row.names of \code{x} are used.
  }
  \item{xlab}{label for horizontal axis
  }
  \item{formula, data}{formula and data for the \code{formula} method
  }
  \item{\dots}{further arguments to the call of \code{plconfint}
  }
}
\details{Columns 4 and 5 of \code{x} are typically used to indicate
  an "overlap interval", which allows for a graphical assessment
  of the significance of the test for zero difference(s),
  akin the "notches" in box plots:
  The difference between a pair of groups is siginificant if their
  overlap intervals do not overlap.
  For equal standard errors of the groups, the standard error of the
  difference between two of them is larger by the factor \code{sqrt(2)}.
  Therefore, the intervals should be shortened by this factor, or
  multiplied by \code{1/sqrt(2)}, which is the default for
  \code{overlapfactor}.
  If only two groups are to be shown, the factor is adjusted to unequal
  standard errors, and accurate quantiles of a t distribution are used.

  The graphical options are:
  \describe{
    \item{\code{lwd}: }{line widths for: [1] the interval,
      [2] middle mark, [3] end marks, [4] overlap interval marks,
      [5] vertical line marking the relevance threshold
    }
    \item{\code{markheight}: }{determines the length of the middle mark,
      the end marks and the marks for the overlap interval
      as a multiplier of the default length}
    \item{\code{extend}: }{extension of the vertical axis beyond the range}
    \item{\code{reflinecol}: }{color to be used for the vertical lines at
      relevances 0 and 1}
  }
}
\value{none
}
\seealso{\code{\link{plot.inference}}
}
\author{Werner A. Stahel
}

%- \seealso{}
\examples{
## --- regression
data(swiss)
rr <- lm(Fertility ~ . , data = swiss)
rt <- termtable(rr)
plot(rt)

## --- termeffects
data(d.blast)
rlm <- lm(log10(tremor)~location+log10(distance)+log10(charge), data=d.blast)
rte <- termeffects(rlm)
plot(rte, single=TRUE)

## --- replication
data(d.osc15Onesample)
td <- d.osc15Onesample
tdo <- structure(td[,c(1,2,6)], names=c("effect", "n", "teststatistic"))
tdr <- structure(td[,c(3,4,7)], names=c("effect", "n", "teststatistic"))
rr <- replication(tdo,tdr)

plconfint(attr(rr, "estimate"), refline=c(0,1))
}
\keyword{hplot}
\concept{relevance}
\concept{replication}
