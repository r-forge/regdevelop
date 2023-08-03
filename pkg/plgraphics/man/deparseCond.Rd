\name{deparseCond}
\alias{deparseCond}

\title{Analyze formula with conditional variables
}
\description{Check if \code{formula} is valid and, if it contains a
  \code{|} character, idenitfy regressors and conditional variables
}
\usage{
deparseCond(formula)
}

\arguments{
  \item{formula}{A model formula, possibly containing a \code{|}
  character that introduces terms describing conditions
}
}
\value{Returns the formula with the following attributes:
\item{y}{"vertical" (response) variable(s)}
\item{x}{"horizontal" (regressor) variable(s)}
\item{a}{(first) conditional variable, if any}
\item{b}{second conditional variable, if any} 
}
\author{Werner Stahel
}
\note{This function is typically used for conditional plots
  and mixed models
}

\examples{
  deparseCond(yy ~ xx)
  deparseCond(yy ~ xx | aa + bb)
  deparseCond(y1 + y2 ~ x1 + log(x2) | sqrt(quantity))

  plyx(Sepal.Width~Sepal.Length | Species, data=iris)
}
\keyword{utilities}
