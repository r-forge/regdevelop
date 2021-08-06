\name{d.everest}
\alias{d.everest}
\docType{data}
\title{Data of an 'anchoring' experiment in psychology
}
\description{
  Are answers to questions influenced by providing partial information? 
  
  Students were asked to guesstimate the height of Mount Everest.
  One group was 'anchored' by telling them that it was
  more than 2000 feet,
  the other group was told that it was less than 45,500 feet.
  The hypothesis was that respondents would be influenced
  by their 'anchor,' such that the first group would produce
  smaller numbers than the second.
  The true height is 29,029 feet.

  The data is taken from the 'many labs' replication study
  (see 'source').
  The first 20 values from PSU university are used here.
}
\usage{data("d.everest")}
\format{
  A data frame with 20 observations on the following 2 variables.
  \describe{
    \item{\code{y}}{numeric: guesstimates of the height}
    \item{\code{g}}{factor with levels \code{low} \code{high}:
      anchoring group}
  }
}
\source{
  Klein RA, Ratliff KA, Vianello M et al. (2014).
  Investigating variation in replicability: A "many labs"
  replication project.
  Social Psychology. 2014; 45(3):142-152.
  https://doi.org/10.1027/1864-9335/a000178
}
%% \references{
%% %%  ~~ possibly secondary sources and usages ~~
%% }
\examples{
data(d.everest)

(rr <- twosamples(log(y)~g, data=d.everest, var.equal=TRUE))
print(rr, show="classical")

pltwosamples(log(y)~g, data=d.everest)
}
\keyword{datasets}
