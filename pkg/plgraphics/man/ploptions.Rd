\name{ploptions}
\alias{ploptions}
\alias{default.ploptions}
\title{Set and Get User "Session" Options that Influence "plgraphics"s Behavior}
\description{
  The user can set (and get) 'pl' options -- mostly graphical "parameters" --
  which influence the behavior \pkg{plgraphics} functions. %%, notably \code{\link{stamp}()}.
}
\usage{
ploptions(x = NULL, ploptions = NULL, list = NULL, default = NULL, 
          assign = TRUE, ...)

default.ploptions
}
\arguments{
  \item{x}{character (vector) of name(s) of ploptions to query.
%%-     option to query (if character) or set (if a named object).
%%-     It may be a character vector of options if it is a query.\cr
%%-    For those ploptions that are lists, \code{x} must be a list, too.
%%-     It contains the components that should be modified, see the second
%%-     example below.
    If \code{x} is set, all further arguments will be ignored.
  }
  \item{ploptions}{the list of options that should be inspected or
    modified. Defaults to \code{usr.ploptions} from the \code{\link{pl.envir}}
    environment. \code{ploptions>1} is equivalent to
    \code{ploptions=pl.envir$ploptions}, the last (or current) list
    used by a high level pl function.}
  \item{list}{a named list of options to be set, see Details}
  \item{default}{character vector of option names.
    These ploptions will be set according to \code{default.ploptions}.
    \code{default="all"} or \code{=TRUE} will reset all options.
    If \code{default} is set, all further arguments will be ignored.
  }
  \item{assign}{logical: should the list be assigned to
    \code{pl.envir$usr.ploptions}? It is then permanant until changed
    again by calling \code{ploptions} again or the session is closed.
    If \code{>1}, the resulting options are stored as
    \code{ploptions} in \code{\link{pl.envir}}, which is changed
    by the high level pl functions.
  }
  \item{...}{any ploptions can be defined or modified,
    using \code{name = value}, as in \code{\link{options}} of basic R.}
}
\value{For \code{ploptions(x)}, where \code{x} is the name of a pl option,
  the current value of the option,
  or \code{NULL} if it is not such a name.
  If \code{x} contains several (valid) names, the respective list.
  
  For \code{ploptions()}, the list of all plptions sorted by name.  
  
  For uses setting one or more options, the important effect is a changed
  list \code{usr.ploptions} in the \code{\link{pl.envir}} environment
  that is used by the package's functions
  (if \code{assign} is \code{TRUE}).
  The (invisibly) returned value is the same list, complemented by an
  attribute \code{"old"} containing the previous values of those options
  that have been changed.
  This list is useful for undoing the changes to restore
  the previous status.
}
\details{
  If the argument \code{list} is set, it must be a named list,
  and each component, with name \code{name} and value \code{value}
  is used as described for arguments in \code{...}, see above
  (in addition to such arguments).

  There is an object \code{ploptions} in the \code{pl.envir}
  environment, which contains the ploptions that have been used
  (usually after modification) by the high level pl function
  last called. This list is used by subsequent calls of lower level pl
  functions. Advanced uses may want to modify this list by assigning to
  \code{pl.envir$ploptions$pch}, for example.

  Here is an incomplete list of the components of \code{default.ploptions},
  describing the suitable alternative values to be set by calling
  \code{ploptions}. For the full set, see
  \code{?\link{ploptions.list}}.
  %% ===========================================================
  \describe{
    \item{keeppar:}{ logical. If TRUE, the graphical parameter settings 
      "mar", "oma", "cex", "mgp", and "mfg" will be maintained when
      leaving high level pl functions, 
      otherwise, the old values will be restored (default).}
    \item{colors:}{ The palette to be used by pl functions}
    \item{csize:}{ General character size, relative to \code{par("cex")}}
    \item{pale:}{ default argument for \code{\link{colorpale}}}
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
    \item{lty, lwd:}{ line type(s) and width(s) }
    \item{col, pcol, lcol:}{ colors to be used generally and
      specifically for points (symbols or text) and lines, respectively,
      given as index of \code{ploptions("colors")}.
      This are often (and by default) vectors to be used for showing
      groups. The first element is usually black.
    }
    \item{colors:}{ the palette to be used}
    \item{censored.pch, censored.csize, censored.pale:}{ ...}
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
      \describe{
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
  }
}
  
\author{Werner A. Stahel}

\seealso{
  \code{\link{stamp}}; \code{\link{ploptions.list}};
  \code{\link{pl.envir}};
  \R's own predefined \code{\link{options}()}.
}
\examples{
## get options
ploptions(c("jitter.factor", "gridlines"))
ploptions("stamp")  ## see example(stamp)
ploptions()  ## all pl options, see '?ploptions.list'

## set options
ploptions(stamp=FALSE, pch=0, col=c.colors[-1], anything="do what you want")
ploptions(c("stamp", "anything"))
ploptions(default=TRUE)  ## reset all pl options, see '?ploptions.list'

## assign to transient options 
t.plopt <- ploptions(smooth.col="purple", assign=2) 
t.plopt$smooth.col
attr(t.plopt, "old")
ploptions("smooth.col") ## unchanged
ploptions("smooth.col", ploptions=2) ## transient options
pl.envir$ploptions["smooth.col"] ## the same

## switching 'margin parameters' between those used
## outside and inside high level pl functions
par(mar=c(2,2,5,2))
plyx(Sepal.Width~Sepal.Length, data=iris, title="The famous iris data set")
par("mar")
mtext("wrong place for text",3,1, col="red")
t.plo <- plmarginpar()
par("mar")
mtext("here is the right place",3,1)

par(attr(t.plo, "oldpar"))  ## resets the 'margin parameters'
par("mar")
plyx(Sepal.Width~Sepal.Length, data=iris, keeppar=TRUE)
par("mar")

## manipulating 'pl.envir$ploptions'
plyx(Sepal.Width~Sepal.Length, data=iris)
pl.envir$ploptions$pch
plpoints(7,4, csize=4)
pl.envir$ploptions$pch <- 4
plpoints(7.5,4, csize=4)

}
\keyword{utilities}
