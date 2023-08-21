\name{ploptions.list}
\alias{ploptions.list}
\title{The List of pl Options}
\description{
  The user can set (and get) 'pl' options -- mostly graphical "parameters" --
  which influence the behavior of \pkg{plgraphics} functions.
}
\usage{
## not used, this gives the complete list of 'pl' options
}
  %% ===========================================================
  \value{
    \itemize{
    \item{keeppar:}{ logical. If TRUE, the graphical parameter settings 
      "mar", "oma", "cex", "mgp", and "mfg" will be maintained when
      leaving high level pl functions, 
      otherwise, the old values will be restored (default).}
    \item{colors:}{ The palette to be used by pl functions}
    \item{csize:}{ General character size, relative to \code{par("cex")}}
    \item{pale:}{ default argument for \code{\link{colorpale}}}
    \item{linewidth:}{ vector of \code{lwd} values to be used for
      the different line types (\code{lty}). The package
      sets \code{lwd} to a value
      \code{ploptions("linewidth")[lty]*lwd} intending to balance
      the visual impact of the different line types, e.g.,
      to allow a dotted line to make a similar impression as a solid
      line.}
    \item{ticklength:}{ vector of 4 scalars: tickmark length,
      corresponding to \code{par("tcl")}. The first 2 elements
      define the length of the regular tickmarks, the other two,
      of the ``small'' tichmarks given by
      \code{attr(ticksat, "small")} (\code{ticksat} is a possible
      attribute of each variable).
      There are two elements each in order to define tickmarks that
      cross the axis.
    }
    \item{tickintervals:}{ vector of length 2. The first element is the
      desired number of tick intervals for axes, to be used as argument
      \code{n} in \code{\link{pretty}}.
      The second determines how many tick labels are shown in the same
      way, and should therefore be smaller than (or equal to) the first.
    }
    \item{pch:}{ plotting symbols or characters}
    \item{csize.pch:}{ size of plotting symbols, relative to default.
      This may be a function with an argument that will be the number of
      observations at the time it is used.}
    \item{csize.plab:}{ size of point labels, relative to \code{csize.pch}}
    \item{psize.max:}{ maximum value of size of plotting symbols}
    \item{lty, lwd, col, pcol, lcol:}{ to be descibed!!!}
    \item{group.lcol:}{ ...}
    \item{variables.pch, variables.col, variables.lty, variables.lcol: }
    { ...}
    \item{censored.pch, censored.size, censored.pale:}{ ...}
    \item{axes:}{ ...}
    \item{axes.sure:}{ ...}
    
    \item{***}{margin parameters.
      \itemize{
	\item{mar,oma}{ ...}
	\item{margin.csize}{ character size for variable labels and
	  tick labels}
	\item{margin.line}{ lines in margin where variable labels and
	  tick labels are shown}
	\item{margin.exp}{ expansion of margins beyond needed lines,
	  for inner and outer margins}
	\item{oma.width}{ ...}
	\item{panelsep}{ space between panels}
      }
    }
    \item{title}{title parameters.
      \itemize{
	\item{title.line}{ line in margin[3] on which the title
	  appears}
	\item{title.adj}{ adjustment of the title}
	\item{title.csize}{ character size of the title, relative to
	  \code{ploptions("csize")*ploptions("margin.csize")[1]}}
	\item{title.csizemin}{ minimum csize}
	\item{title.maxchars}{ maximum number of characters in title}
	\item{sub}{ logical: should subtitle be shown?}
      }
    }
    \item{panel}{ panel function to be used in high level pl functions}
    
    \item{gridlines:}{  can be \cr 
      -- a logical indicating if gridlines should be drawn. If
      \code{TRUE}, gridlines will be drawn at the values given in
      \code{attr(.,"ticksat")};
      -- a vector of values at which the gridlines should appear;\cr
      -- a list of length 2 of such values;\cr
      -- a named list. If a name equals the attribute \code{varname}
      of either the x or y variable, the respective component will be
      used.
    }
    \item{\code{smooth.lty, smooth.col}:}{ line type and color.
      Note that if there is a \code{smooth.group} factor,
      \code{group.lty} and \code{group.col} are used.
    }
    \item{\code{smooth.lwd}:}{ line width. If of length 2 (or more),
      the second element is the factor by which the line width is
      reduced for simulated smooths (that is, for the second to the last
      column of \code{smoothline$y}). It defaults to 0.7.
    }
    \item{smooth.xtrim:}{ proportion of fitted values to be trimmed off on
      both sides when drawing a smooth line, either a number or a function
      that takes the number of points as its argument.
      The default is the simple function \code{2^log10(n)/n}.
      The smoothing function may produce an attribute \code{xtrim}
      that is used as an additional factor to \code{smooth.xtrim}.
      This is applied, e.g., to suppress trimming if a straight line is
      fitted instead of a smooth by requiring \code{smoothLm} as the
      smoothing function.
    }
    \item{smooth.minobs:}{ minimal number of observations needed for
      calculating a smooth.}
    \item{smooth.band:}{ Indicator (logical) determining
      whether "low" and "high" smooth lines should be drawn.
      See above for their definition.}
    \item{condquant...:}{ Conditional quantiles for censored residuals.
      \itemize{
	\item{condquant:}{ logical: should bars be drawn for censored residuals?
	  If \code{FALSE}, censored observations will be set to the median of
	  the conditional distribution and shown by a different plotting
	  character, see argument \code{censored} of
	  \code{\link{ploptions}}.
	  If \code{NULL}, the standard plotting character will be used.
	}
	\item{condquant.probrange:}{ range for probabilities.
	  If the probability corresponding to the censored part of the
	  distribution is outside the range, bars will not be drawn.}
	\item{condquant.pale:}{ factor by which the \code{pcol} color
	  will be paled to show the points (\code{condquant.pale[1]})
	  and the bars (\code{...[2]}).
	}
      }
    }
    \item{plcond...:}{ features of \code{\link{plcond}}.
      \itemize{
	\item{plcond/panel:}{ panel function to be used}
	\item{plcond.nintervals:}{ number of intervals into which
	  numerical variables will be cut}
	\item{plcond.extend:}{ proportion of neighboring intervals for
	  which points are shown. 0 means no overlap.}
	\item{plcond.col:}{ 4 colors to be used to mark the points of
	  the neighboing intervals:
	  The first and second ones color the points lower or higher
	  than the interval of the horizontal conditioning variable,
	  and the other two regulating the same features for the
	  vertical variable.
	  The points which are outside the intervals of both
	  conditioning variables will get a mixed color.}
	\item{plond.pale:}{ minimum and maximum paling, to be applied
	  for distance 0 and maximal distance from the interval.}
	\item{plcond.cex:}{ symbol size, relative to \code{cex}, used to
	  show the points outside the interval}
      }
    }
  }
}
  
\author{Werner A. Stahel}

\seealso{
  \code{\link{ploptions}}
}
\examples{
names(default.ploptions)
}
\keyword{utilities}

