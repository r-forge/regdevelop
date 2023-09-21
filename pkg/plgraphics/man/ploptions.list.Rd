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
    \describe{
    \item{keeppar:}{ logical. If TRUE, the graphical parameter settings 
      "mar", "oma", "cex", "mgp", and "mfg" will be maintained when
      leaving high level pl functions, 
      otherwise, the old values will be restored (default).}
    \item{colors:}{ The palette to be used by pl functions}
    \item{pale:}{ default argument for \code{\link{colorpale}}}
    \item{linewidth:}{ vector of \code{lwd} values to be used for
      the different line types (\code{lty}). The package
      sets \code{lwd} to a value
      \code{ploptions("linewidth")[lty]*lwd} intending to balance
      the visual impact of the different line types, e.g.,
      to allow a dotted line to make a similar impression as a solid
      line.}
    \item{csize:}{ General character size, relative to \code{par("cex")}}
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
    \item{lty, lwd, col, pcol, lcol:}{ line type, line width, color to
      be used}
    \item{pcol, lcol:}{ color to be used for plotting symbols and
      labels, respectively}
    \item{***}{innerrange}
    \describe{
      \item{innerrange}{ logical: should an innerrange be used in plots
	if needed?}
      \item{innerrange.factor}{ factor needed to determined the inner
	range}
      \item{innerrange.ext}{ extension of the inner range}
      \item{innerrange.function}{ function used to calculate the inner
	range}
    }
    \item{plext}{ extension of the data range to the plotting range}
    \item{plsymbolext}{ further extension to allow for large symbols
      near the limits of the plotting range}
    \item{markextremes}{ proportion of observations to be marked by
      their labels on the lower and upper extremes}
    \item{variables.pch, variables.col, variables.lty,
      variables.lcol:}{ vectors of symbols, color, line type, line color
      to be used for showing different y variables}
    \item{censored.pch, censored.size, censored.pale:}{ plotting symbol
      and size, and pale value to be applied to censored observations.
      Different symbols are used for distinguishing right and left
      censoring in vertical and horizontal direction and there
      combination.
    }
    \item{group.pch, group.col, group.lty, group.lcol:}{ vector of
      colors used for lines in the different groups ...} %% !!!
    \item{***}{title parameters.
      \describe{
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
    \item{xlab, ylab}{ labels of x and y axes}
    \item{mframesmax}{ maximum number of panels to be shown on one page}
    \item{panel}{ panel function to be used in high level pl functions}

    \item{axes:}{ axes to be shown}
    \item{axes.sure:}{ !!!}
    \item{***}{margin parameters.
      \describe{
	\item{mar, oma}{ ...}
	\item{mar.default, oma.default}{ their default values}
	\item{margin.csize}{ character size for variable labels and
	  tick labels}
	\item{margin.line}{ lines in margin where variable labels and
	  tick labels are shown}
	\item{margin.exp}{ expansion of margins beyond needed lines,
	  for inner and outer margins}
	\item{panelsep}{ space between panels}
      }
    }

    \item{***}{date parameters.
      \describe{
	\item{date.origin}{ The year which serves as origin of the
	  internal (julian) date scale}
	\item{date.format}{ format for showing dates}
	\item{timerangelim:}{ ...} %% !!!
	\item{date.ticks}{ data.frame ruling
	  how many small and large ticks and tick labels will be shown.
	  The first column determines the row that will be used}
      }
    }
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
    \item{zeroline:}{ logical: should zero (0) be shown be a special
      grid line?
      Can be numerical, then gives coordinates of such lines,
      generalizing the zero line.}
    \item{zeroline.lty, zeroline.lwd, zeroline.col}{ line type, width
      and color of the zero line}
    \item{refline}{ reference line, any line to be added to the current
      plot using the following properties. May be given as ...!!!}
    \item{refline.lty, refline.lwd, refline.col}{ line type, width
      and color of the ref line}
    \item{***}{smooth.
      \describe{
	\item{smooth}{logical: should a smoothing line be shown?}
	\item{smooth.function:}{ function for calculating the smoother}
	\item{smooth.par, smooth.iter}{parameters for the function}
	\item{smooth.minobs:}{ minimal number of observations needed for
	  calculating a smooth.}
	\item{smooth.band:}{ Indicator (logical) determining
	  whether "low" and "high" smooth lines should be drawn.
	  See above for their definition.}
	\item{\code{smooth.lty, smooth.col}:}{ line type and color.
	  Note that if there is a \code{smooth.group} factor,
	  \code{group.lty} and \code{group.col} are used.}
	\item{\code{smooth.lwd}:}{ line width. If of length 2 (or more),
	  the second element is the factor by which the line width is
	  reduced for simulated smooths (that is, for the second to the last
	  column of \code{smoothline$y}). It defaults to 0.7.}
	\item{smooth.pale}{paling factors to be applied to ...} %% !!!
	\item{smooth.xtrim:}{ proportion of fitted values
	  to be trimmed off on both sides when drawing a smooth line,
	  either a number or a function
	  that takes the number of points as its argument.
	  The default is the simple function \code{2^log10(n)/n}.
	  The smoothing function may produce an attribute \code{xtrim}
	  that is used as an additional factor to \code{smooth.xtrim}.
	  This is applied, e.g., to suppress trimming if a straight line is
	  fitted instead of a smooth by requiring \code{smoothLm} as the
	  smoothing function.
	}
      }
    }
    \item{bar.midpointwidth}{width of the line shown at the central
      point of a bar}
    \item{bar.lty, bar.lwd, bar.col}{line type, width (for bar and
      midpoint line), color of bars}
    \item{***}{factors, multibox plots:
      \describe{
	\item{factor.show:}{how should factors be plotted.
	  Options are ...} %% !!!
	\item{mbox.minobs}{minimal number of observations shown as a
	  multibox plot}
	\item{mbox.minheigth}{...} %% !!!
	\item{mbox.colors}{colors to be used for multibox plots}
	\item{jitter}{amount of jitter} %% !!!
	\item{jitter.minobs}{minimal number of observations to which
	  jittering should be applied}
	\item{jitter.factor}{...} %% !!!
	
      }
    }
    \item{***}{condquant: Conditional quantiles for censored residuals.
      \describe{
	\item{condquant:}{ logical: should bars be drawn
	  for censored residuals?
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
    \item{***}{plcond: features of \code{\link{plcond}}.
      \describe{
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
    \item{subset.rgratio}{adjust plot range for a subset if the range is
      smaller than \code{subset.rgratio} times the plot range for the full
      data set}
    \item{functionxvalues}{if a function is to be shown, the number of
      argument values for which the function is evaluated}
    \item{***}{options for the function \code{plregr}
      \describe{
	\item{regr.plotselect}{ selection of diagnostic plots that are
	  produced, see ...} %% !!!
	\item{regr.addcomp}{ should residuals be shown as they are or
	  component effects added to them?}
	\item{leveragelimit}{ ...} %% !!!
	\item{cookdistancelines}{values of Cook's distance for which
	  contours will be shown on the leverage plot}
      }
    }
    \item{stamp}{logical: should stamps be shown in the bottom right
      concern documenting the date and any project and step titles?}
    \item{doc}{logical: should any documentations of the data set
      be shown as subtitles, i.e., at in the top margin of the plot?}
    \item{printnotices:}{ logical: should notices produced by the
      functions be shown?}
    \item{debug:}{ Some functions that produce nice-to-have features
      are prevented from aborting the process if they fail
      (by using the \code{try} function) and produce a warning instead
      -- unless \code{debug} is \code{TRUE}
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

