\name{d.negposChoice}
\alias{d.negposChoice}
\docType{data}
\title{Data of an 'anchoring' experiment in psychology
}
\description{
  Is a choice influenced by the formulation of the options?

  Here is the question:
  Confronted with a new contagious disease, the government has
  a choice  between
  action A that would save 200 out of 600 people or
  action B which would save all 600 with probability 1/3.
  This was the 'positive' description.
  The negative one was that either (A) 400 would die or
  (B) all 600 would die with probability 2/3.

  The dataset encompasses the results for
  Penn State (US) and Tilburg (NL) universities.
}
\usage{data("d.negposChoice")}
\format{
  A data frame with 4 observations on the following 4 variables.
  \describe{
    \item{\code{uni}}{character: university}
    \item{\code{negpos}}{character: formulation of the options}
    \item{\code{A}}{number of students choosing option A}
    \item{\code{B}}{number of students choosing option B}
  }
}
\source{
  Klein RA, Ratliff KA, Vianello M et al. (2014).
  Investigating variation in replicability: A "many labs"
  replication project.
  Social Psychology. 2014; 45(3):142-152.
  https://doi.org/10.1027/1864-9335/a000178
}
\examples{
data(d.negposChoice)

d1 <- d.negposChoice[d.negposChoice$uni=="PSU",-1]
(r1 <- twosamples(table=d1[,-1]))
d2 <- d.negposChoice[d.negposChoice$uni=="Tilburg",-1]
r2 <- twosamples(table=d2[,-1])

}
\keyword{datasets}
