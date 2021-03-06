\name{gendateaxis}
\alias{gendate}
\alias{gendateaxis}

\title{Generate a variable expressing time with its attributes for plotting
}
\description{\code{gendateaxis} generates suitable attributes for
  plotting a date or time variable.\cr
  \code{gendate} generates a date variable and is an extension of
  \code{\link{as.POSIXct}}.
}
\usage{
gendate(date = NULL, year = 2000, month = 1, day = 1, hour = 0, 
  min = 0, sec = 0, data = NULL, format = "y-m-d", origin = NULL)

gendateaxis(date = NULL, year = 2000, month = 1, day = 1, hour = 0,
  min = 0, sec = 0, data = NULL, format = "y-m-d", origin = NULL,
  ploptions=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{date}{vector of class \code{dates} or \code{chron} or vector
    to be converted into such an object. May also be the name of such
    a variable contained in \code{data}.
    If \code{date} has class \code{dates} or \code{chron}, the arguments
    \code{year, month} and \code{day} are ignored. 
  } 
  \item{year, month, day, hour, min, sec}{numeric vectors giving the
    year, month, day of month, hour, minute, second -- or the name of such a
    variable contained in \code{data}.
    \code{day, hour, min, sec} can be fractional, see Details.
    If these arguments are used, the supersede the respective parts in
    \code{date}.
  }
%%   \item{mon, mday}{numeric vectors giving the month and the day of
%%     month minus 1, as in the POSIXt format.
%%     (If \code{month} or \code{day} is also given, it supersedes \code{mon}
%%     or \code{mday}.)
%%   }
  \item{data}{data.frame, where variables can be found
  }
  \item{format}{format for \code{date} in case that the latter is
    a character vector
  }
  \item{origin}{year of origin for dates, defaults to
    \code{ploptions("date.origin")}
  }
  \item{ploptions}{list pl options, generated by \code{pl.control}
  }
}
\details{
  If \code{hour} is fractional, e.g., 6.2, the fraction is respected,
  that is, it will be the same as time \code{06:12}.
  If \code{min} is also given, the fraction of \code{hour} is ignored.
  Similar for \code{day} and \code{min}.
  \cr
  If \code{hour} is \code{>=24}, the \code{day} is augmented by
  \code{hour\%/\%24} and the hour is set to \code{hour\%\%24}.
  Similar for \code{min} and \code{sec}.
}  
\value{
  For \code{gendate}, a vector of times in \code{POSIXct} format.\cr
  For \code{gendateaxis}, this is augmented by the attribute
  \item{numvalues}{numerical values used for plotting.
    If years, months or days vary in the data, the units are days.
    Otherwise, they are hours, minutes, or seconds, depending on the
    highest category that varies.
  }
  Unless the dates only cover one of the categories
  (only years differ, or only months, ...), the following plotting
  attributes are added:
  \item{ticksat}{vector where tickmarks are shown.
  It contains its own attribute \code{small} if secondary ticks are
  suitable.} 
  \item{ticklabels}{May be years, quarters, month names, days, ...}
  \item{ticklabelsat}{vecor of coordinates to place the ticklabels}
  \item{label}{equals "", since the time scale makes it clear enough
    that the axis represents time.}
}
\author{Werner A. Stahel
}

\seealso{\code{\link{genvarattributes}, \link{axis.Date}}
  %%  \link[chron]{axis} %% in package \code{chron}
}
\examples{
## call gendateaxis without 'real' data
tt <- gendate(year=rep(2010:2012, each=12), month=rep(1:12, 3))
ta <- gendateaxis(tt)

## ... derived from data
data(d.river)
d.river$dt <- gendateaxis(date="date", hour="hour", data=d.river)
plyx(O2~dt, data=d.river, subset=months(date)!="Sep")
plyx(O2~dt, data=d.river[months(d.river$date)!="Sep",])
plyx(O2~dt, data=d.river, subset=1:1000)
}
\keyword{utilities}

