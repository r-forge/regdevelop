\name{plmatrix}
\alias{plmatrix}
\alias{panelDefault}
\alias{panel.smooth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Scatterplot Matrix}
\description{
  Plots a scatterplot matrix, for which the variables shown horizontally
  do not necessarily coincide with those shown vertically. If desired,
  the matrix is divided into several blocks such that it fills more than
  1 plot page.

  code{panelDefault} is the function to be applied for each subplot,
  unless a different function is provided. 
}
\usage{
plmatrix(x, y = NULL, data = NULL, panel = panelDefault,
  nrow = 0, ncol = nrow, save = TRUE, robrange. = FALSE, range. = NULL,
  pch = 1, col = 1, reference = 0, ltyref = 3,
  log = "", xaxs = "r", yaxs = "r",
  xaxmar = NULL, yaxmar = NULL, xlabmar=NULL, ylabmar=NULL,
  vnames = NULL, main = "", xlab = NULL, ylab = NULL,
  cex = NA, cex.points = NA, cex.lab = 1, cex.text = 1.3, cex.title = 1,
  bty = "o", oma = NULL, mar = rep(0.2, 4), keeppar = FALSE,
  axes = TRUE, ...)  %% ask=par("ask"), 

panelDefault(xx, yy, indx, indy, pch=par("pch"), col=par("col"), 
  cex=par("cex"), size=NULL, ...)

panel.smooth(xx, yy, indx, indy, pch=par("pch"), col=par("col"),
  cex=par("cex"), ...)
%% args xx and yy instead of x and y in order to distinguish them here
%%  in the .Rd file
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{data for columns (x axis), or formula defining column
    variables. If it is a formula containing a left hand side,
    the left side variables will be used last.}
  \item{y}{data or formula for rows (y axis). Defaults to \code{x}}
  \item{data}{data.frame containing the variables in case \code{x}
    or \code{y} is a formula}
  \item{panel}{a function that generates the marks of the individual
    panels, see Details.
    Defaults essentially to \code{\link{points}} or \code{\link{text}}
    depending on the argument \code{pch},
    to \code{\link{plmboxes}} if  'x'  is a factor and  'y'  is not,
    or to a modification of \code{sunflowers} if both are factors.
  }
  \item{nrow}{number of rows of panels on a page}
  \item{ncol}{number of columns of panels on a page}
  \item{save}{if y is not provided and \code{save==TRUE},
    the first row and the last column are suppressed.}
  \item{robrange.}{if TRUE, robust plot ranges will be used}
  \item{range.}{plot ranges, given as a matrix with 2 rows (min, max)
    and \code{colnames} identifying the variables.}
  \item{pch}{plotting character. A vector of integers, characters or
    strings can also be given for the default panel function}
  \item{col}{color(s) to be used for plotting the observations}
  \item{reference}{coordinates for reference lines to be shown in the
    panels. A named vector can be used to define a value for each or
    any variable.}
  \item{ltyref}{line type for reference lines}
  \item{log}{specifies logarithmic scale of axes. \code{"x"} asks for
    log scale on horizontal axis, \code{"y"}, on vertical axis,
    \code{"xy"}, on both axes.}
  \item{xaxs, yaxs}{styles for x and y axis, see \code{\link{par}}}
  \item{xaxmar, yaxmar}{in which margin should the x- [y-] axis be
    tick-marked?}
  \item{xlabmar, ylabmar}{in which margin should the x- [y-] axis be
    labelled?}
  \item{vnames}{labels for the variables}
  \item{main}{main title for the plot (to be repeated on each plot
    page)}
  \item{xlab, ylab}{vectors of labels for x- and y-axes, a string for
    each variable. This will override the \code{vnames} argument for the
    axis labels, but not for the diagonal panels.}
  \item{cex, cex.points}{character expansion for showing the
    observations (both arguments are equivalent, cex has priority).
  }
  \item{cex.lab, cex.text}{character expansion for variable labels
    in the margin and in the "diagonal", respectively,
    relative to \code{cex}}
  \item{cex.title}{character expansion for the main title}
  \item{bty}{box type for each panel, see \code{\link{par}}}
  \item{oma}{width of outer margins, ee \code{\link{par}}}
  \item{mar}{margins for the panels}
  \item{keeppar}{logical. If TRUE, the graphical settings
    "mfrow", "mar", "cex", and "mgp" will be maintained, otherwise,
    the old values will be restored (default).}
  \item{axes}{logical. If TRUE, axes will be drawn and labels shown
    (default).}
%%-   \item{ask}{Should R ask before erasing the screen?
%%-     The default is 'getOption('ask'), followed by TRUE for certain
%%-     graphical devices.}
  \item{\dots}{further arguments passed to the \code{panel} function
    and possibly further to functions called by the panel function\cr}

  Arguments for \code{panelDefault} and \code{panel.smooth}
  \item{xx}{values of the horizontal variable}
  \item{yy}{values of the vertical variable}
  \item{indx}{index of the variable shown horizontally, among the
    \code{y} variables}
  \item{indy}{index of the variable shown horizontally, among the
    \code{y} variables}\cr
  \code{pch, col}{plotting character and color, as usual}
  \item{size}{size of sunflowers for plotting two factors}
}
\details{
  The \code{panel} function can be user written. It needs \eqn{>=6}
  arguments which must correspond to the arguments of
  \code{panelDefault}, see above. If some arguments are not used,
  just introduce them as arguments to the function anyway in order to
  avoid (unnecessary) error messages and stops.
  The names \code{pch} and \code{col} must appear among the argument
  names.

  \code{panel.smooth} only calls \code{\link{panel.smooth}} of the
  \code{graphics} package, 
  absorbing the arguments that are needed but not used.

  Since large scatterplot matrices lead to tiny panels, \code{plmatrix}
  splits the matrix into blocks of at most \code{nrow} rows and
  \code{ncol} columns. If these numbers are missing, they default to
  \code{nrow=5} and \code{ncol=6} for landscape pages, and to
  \code{nrow=8} and \code{ncol=5} for portrait pages.
}
\value{
  none
}
\author{Werner A. Stahel, ETH Zurich}
\seealso{\code{\link{pairs}}}
\examples{
plmatrix(iris[,1:4], main="Iris", pch=as.numeric(iris[,"Species"]))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
