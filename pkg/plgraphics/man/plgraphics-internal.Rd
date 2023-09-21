\name{plgraphics-internal}
\alias{is.formula}
\alias{RNAMES}
\alias{\%nin\%}
\alias{u.true}
\alias{u.false}
\alias{u.notfalse}
\alias{i.def}
\alias{u.debug}
\alias{DB}
%% ---
\title{Internal 'plgraphics' Objects}
\description{
  Internal functions of package \pkg{plgraphics} that are exported, but
  will normally not be called by the user directly.
  They mostly are simple shortcuts, combining two or a few R commands.
  \describe{
    \item{is.formula}{Check if argument is of \code{mode} \code{formula}}
    \item{RNAMES}{code{row.names} if \code{x} is a matrix or data.frame,
      otherwise give the \code{names} back}
    \item{\%nin\%}{check which elements of \code{x} are not contaied in
      \code{y}}
    \item{u.true, u.false, u.notfalse}{check if argument is logical
      and \code{TRUE, ...}}
    \item{i.def}{define: if the first argument is
      either null or \code{NA}, return \code{value},
      if it is \code{TRUE}, return \code{valuetrue},
      if it is \code{FALSE}, return \code{valuefalse}}      
    \item{u.debug}{check if \code{ploptions("debug")} is \code{TRUE}}
    \item{DB}{set debug level and action:
      if argument \code{on} if \code{TRUE} or \code{>=1},
      call \code{browser} on error,
      if \code{on>1}, additionally convert warnings into errors,
      if \code{on==0}, reset to usual error and warning handling
    }
  }
}

\usage{
is.formula(object)
RNAMES(x)
x \%nin\% y
u.true(x)
u.false(x)
u.notfalse(x)
i.def(arg, value = TRUE, valuetrue = value, valuefalse = FALSE)
u.debug()
DB(on=TRUE)
}
\arguments{
  \item{x,y, object}{\R objects}
  \item{on}{logical}
  \item{arg, value, valuetrue, valuefalse}{see above}
}
\value{see description of purpose}
\examples{
i.def(NA, 10, 20, 30)  ## 10
i.def(TRUE, 10)  ## 10
i.def(TRUE, 10, 20)  ## 20
i.def(FALSE, 10, 20, 0)  ## 0

u.true(1) ## FALSE, since 1 is not of mode logical
u.notfalse(2) ## TRUE
u.notfalse(FALSE) ## FALSE
}
\keyword{internal}
