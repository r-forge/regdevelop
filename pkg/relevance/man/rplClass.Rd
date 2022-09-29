\name{rplClass}
\alias{rplClass}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reproducibility  Class
}
\description{
  Find the classes of relevance and of reprodicibility.
}
\usage{
rplClass(rlvclassd, rlvclassr, rler=NULL)
}

\arguments{
  \item{rlvclassd}{relevance class of the difference between rplication
  and original study
  }
  \item{rlvclassr}{relevance class of the replication's effect estimate
  }
  \item{rler}{estimated relevance of the replication
  }
}
\value{Character string: the replication outcome class
}
\references{Werner A. Stahel (2020).
  Measuring Significance and Relevance instead of p-values.
  Submitted
}
\author{Werner A. Stahel
}

\examples{
data(d.osc15Onesample)
tx <- structure(d.osc15Onesample[,c("effecto","teststatistico","no")],
      names=c("effect","teststatistic","n"))
ty <- structure(d.osc15Onesample[,c("effectr","teststatisticr","nr")],
      names=c("effect","teststatistic","n"))
rplClass(tx, ty)
}
\keyword{documentation}
\keyword{misc}
