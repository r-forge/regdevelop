\name{getvariables}
\alias{getvariables}
\alias{getvarnames}
\title{Extract Variables or Variable Names
}
\description{
  \code{getvarnames} extracts the variables' names 
  occurring in a formula, in raw form
  (as \code{get\_all\_vars}) or in transformed form
  (as \code{model.frame} does it).

  \code{getvariables} collects variables from a data.frame
}
\usage{
getvariables(formula, data = NULL, transformed = TRUE,
  envir = parent.frame(), ...)
getvarnames(formula, data = NULL, transformed = FALSE)
}
\arguments{
  \item{formula}{a model 'formula' or 'terms' object or an R object,
    or a character vector of variable names}
  \item{data}{a data.frame, list or environment (or object coercible by
    'as.data.frame' to a data.frame), containing the variables in
    'formula'.  Neither a matrix nor an array will be accepted.}
  \item{transformed}{logical. If \code{TRUE}, variables will be extracted as
    transformed in \code{formula}, otherwise, untransformed variables
    are returned.}
  \item{envir}{environment in which the \code{formula} will be evaluated
    }
  \item{\dots}{further arguments such as
    \code{data, weight, subset, offset}
    used to create extra columns in the resulting data.frame, with
    names between dots such as '".offset."'}
}
\value{
  For \code{getvarnames}:
    names of all variables (\code{transformed=FALSE})
    or simple terms (\code{transformed=TRUE}),
    including the attributes 
  \item{xvar}{those from the right hand side of the formula}
  \item{yvar}{left hand side, if present}
  \item{yvar}{conditioning part, denoted after a \code{|} symbol
    in formula, if applicable} 

  For \code{getvariables}: 
  data.frame containing the extracted variables or simple terms,
  with the attributes of \code{getvarnames}
}
\author{
Werner A. Stahel
}

\seealso{
  \code{\link{model.frame}}, \code{\link{get_all_vars}}
}
\examples{
data(d.blast)
getvarnames(log10(tremor)~log10(distance)*log10(charge), data=d.blast)

dd <- getvariables(log10(tremor)~log10(distance)*log10(charge),
                   data=d.blast, by=location)
str(dd)
}
\keyword{auxiliary}

