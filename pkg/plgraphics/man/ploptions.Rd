\name{ploptions}
\alias{ploptions}
\alias{.ploptions}
\alias{ploptionsDefault}
\title{Set and Get User "Session" Options that influence "regr0"s behavior}
\description{
  The user can set (and get) "Session" options which influence the
  behavior \pkg{regr0} functions, notably \code{\link{stamp}()}.
}
\usage{
ploptions(x = NULL, default = NULL, list = NULL, ploptions = NULL,
          assign = TRUE,...)
.ploptions
ploptionsDefault
}
\arguments{
  \item{x}{option to query (if character) or set (if a named object).
    It may be a character vector of options for if it is a query.\cr
    For those ploptionsions that are lists, \code{x} must be a list, too.
    It contains the components that should be modified, see the second
    example below.
  }
  \item{list}{a named list of options to be set}
  \item{default}{default value for \code{getPloptions}.
    If \code{NULL}, the option will be set according to
    \code{ploptionsDefault}.}
  \item{ploptions}{the list of options that should be modified}
  \item{assign}{logical: should the list be assigned to
    \code{.ploptions}?}    
  \item{...}{any ploptions can be defined or modified,
    using 'name = value'.}
}
\value{For 'ploptions()', a list of all set UserOptions sorted by name.  

  For 'ploptions(name)', a \emph{list} of length one containing the set
  value, or 'NULL' if it is not set.

  For uses setting one or more options, the important effect is a changed
  list \code{.ploptions} that is used by the \code{pl} graphical
  functions (if \code{assign} is \code{TRUE}).
  The (invisibly) returned value is the same list, complemented by an
  attribute \code{"old"} containing the previous values of those ploptions
  that have been changed.
  This list is useful for undoing the changes, usually at the end of
  graphical functions, to restore the previous status.

  \code{.ploptions} and \code{ploptionsDefault} are lists of options.
  The first one is generated whenever ploptions are set
  (and \code{assign} is \code{TRUE}), and is then
  stored in the \code{".GlobalEnv"} environment.
  The second is stored in the package \code{plgraphics}.
}
\author{Werner A. Stahel}
\note{In contrast to the \code{\link{options}} of R base,
  the \code{UserOptions} are stored on the \code{.GlobalEnv}
  (if anyone is changed)
  and will be recovered if the latter is stored and loaded again.
}
\seealso{
  \code{\link{stamp}}; \R's own predefined \code{\link{options}()}.
}
\examples{
## see example(stamp)
ploptions("stamp")
ploptions(c("jitterFactor", "gridlines"))

## set options
ploptions(stamp=FALSE, basic.pch=0, basic.col=c.colors[-1])

ploptions("stamp")
}
\keyword{utilities}