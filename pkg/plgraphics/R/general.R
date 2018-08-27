## ======================================================================
showd <- function(data, first=3, nrow.=4, ncol.=NULL)
{
  ## print some rows (and columns) of a matrix or data.frame
  if (length(data)==0) {
    cat("Null object: ", str(data), "\n")
    return()
  }
  ldoc <- getOption("doc")
  if (length(ldoc)>0 && ldoc && length(tit(data))>0) {
    cat("tit: ",tit(data),"\n")
  }
  lldim <- length(dim(data))
  if (lldim>2) stop("!showd not yet programmed for arrays")
  if (lldim>0) cat("dim: ",dim(data),"\n") else
    if (is.factor(data)) data <- as.character(data)
  ldata <- cbind(data)
  l.nr <- nrow(ldata)
  l.nc <- ncol(ldata)
  if (is.null(colnames(ldata))) colnames(ldata) <- paste("c",1:l.nc,sep=".")
  ## select columns
  l.ic <- if (length(ncol.)==0) 1:l.nc  else {
    if (length(ncol.)==1) {
      if (l.nc>ncol.)
        c(seq(1,by=l.nc%/%ncol.,length=ncol.-1),l.nc) else 1:l.nc
    } else  {
      lic <- ncol.[ncol.>0&ncol<=l.nc]
      if (length(lic)>0) lic else 1:l.nc
    }
  }
  ## select rows
  if (l.nr<=nrow.+first)  l.dc <- format(ldata[,l.ic, drop=FALSE])  else {
    l.ir <- c(1:first,round(seq(first,l.nr,length=nrow.+1))[-1])
    l.ir <- unique(c(last(l.ir,-1),l.nr))
    l.dc <- data.frame(u.merge(format(ldata[l.ir,l.ic]),"",after=first),
                       stringsAsFactors=FALSE)
    names(l.dc) <- colnames(ldata)[l.ic]
    lrn <- row.names(ldata)
    if (is.null(lrn)) lrn <- paste("r",1:l.nr,sep=".")
    row.names(l.dc) <- c(lrn[1:first],"...", lrn[l.ir[-(1:first)]])
  }
  ## was vector or array with only 1 column
  if (l.nc==1) {
    if (lldim>0) cat("     transposed column\n")
    row.names(l.dc) <-
      format(rbind(row.names(l.dc),l.dc[,1]),justify="right")[1,]
    l.dc <- t(l.dc)
  }
  print(l.dc,quote=FALSE)
  if (length(ldoc)&&ldoc&&length(doc(data)))
    cat("\ndoc:  ",paste(doc(data),collapse="\n  "),"\n")
  invisible(l.dc)
}
## ===================================================
sumna <- function(object,inf=TRUE)
{
  ## Purpose:   count NAs along columns
  ## ----------------------------------------------------------------------
  ## Arguments:
  ##   object   data.frame, matrix or vector
  ##   inf      treat Inf as NA
  ## ----------------------------------------------------------------------
  ## Author: Werner Stahel, Date: 10 Oct 2007, 08:18
  ff <- if(inf) {
    function(x)
    if(is.numeric(x)) sum(!is.finite(x)) else sum(is.na(x)) }
      else function(x) sum(is.na(x))
  if (is.matrix(object)) apply(object,2,ff)  else {
    if (is.list(object)) sapply(object,ff)
    else if(is.atomic(object)) ff(object)
  }
}
## ==========================================================================
robrange <-
  function(data, trim=0.2, fac=5.0, na.rm=TRUE)
{
  fac <- i.def(fac, 5, valuefalse=5)
  c.trimcoef0 <- 0.74
  c.trimcoef1 <- -0.87
  lna <- any(!is.finite(data))
  if (lna) {
    if(!na.rm) stop("!robrange! 'data' contains NAs")
    data <- data[is.finite(data)]
  }
  ln <- length(data)
  lsdexpected <- (c.trimcoef0 + c.trimcoef1*trim)
  if (is.character(data)|length(data)==0) {
    warning(":robrange: invalid data. Returning NA")
    return(c(NA,NA))
  }
  trim <- i.def(trim, 0.2)
  lmn <- mean(data,trim=trim)
  lds <- sort(abs(data-lmn))
  lnt <- ceiling((1-trim)*ln)
  if (lnt<3 | lnt==ln) {
    warning(":robrange: not enough valid data. returning ordinary range")
    lsd <- Inf } else {
    lsd <- fac*sum(lds[1:lnt]/(lnt-1)) / lsdexpected
    if (lsd==0) {
      warning(":robrange: robust range has width 0. returning ordinary range")
      lsd <- Inf }
               }
  structure(c(max(lmn-lsd,min(data)), min(lmn+lsd,max(data))),
            location=lmn, scale=lsd)
}
## =======================================================================
quinterpol <- function(x, probs = c(0.25,0.5,0.75), extend=TRUE)
{
  ## Purpose:
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: Werner Stahel, Date: 15 Nov 2014, 16:04
  lx <- x[!is.na(x)]
  ltb <- table(lx)
  ln <- length(lx)
  lnn <- length(ltb)
  ln1 <- lnn+1
  lxx <- as.numeric(names(ltb))
  lxm <- (lxx[-1]+lxx[-lnn])/2
  lx0 <- if(extend) 2*lxx[1]-lxm[1] else lxx[1]
  lx1 <- if(extend) 2*lxx[lnn]-lxm[lnn-1] else lxx[lnn]
  lxe <- c(rbind(c(lx0,lxm),lxx),lx1)
  lp <- c(0,cumsum(ltb)/ln)
  lpp <- (lp[-1]+lp[-ln1])/2
  lpe <- c(rbind(lp,c(lpp,1)))  ## last element (1) is ineffective
  ld <- outer(probs,lpe,"-")
  li <- apply(ld>0,1,sum)
  lii <- 1:length(probs)
  ldd <- cbind(ld[cbind(lii,li)],ld[cbind(lii,li+1)])
  lh <- ldd[,1]/(ldd[,1]-ldd[,2])
  lxe[li]*(1-lh) + lxe[li+1]*lh
}
## =======================================================================
quantilew <- function(x, probs=c(0.25,0.5,0.75), weights=1, na.rm=FALSE)
{
  ## Purpose:   quantile with weights, crude version
  ## -------------------------------------------------------------------------
  ## Arguments:
  ## -------------------------------------------------------------------------
  ## Author: KSK Projekt, Date: 14 Dec 1999, 12:02
  probs <- probs[!is.na(probs)]
  if (length(weights)==1) return(quantile(x, probs))
  if (length(weights)!=length(x))
    stop("!quantilew! lengths of 'x' and 'weights' must be equal")
  if (any(t.ina <- is.na(x))) {
    if (!na.rm) stop("!quantilew! NAs not allowed while 'na.rm' is FALSE")
    x <- x[!t.ina]
    weights <- weights[!t.ina]
  }
  t.i <- order(x)
  t.d <- x[t.i]
  t.wg <- cumsum(weights[t.i])/sum(weights)
  t.i1 <- apply(outer(t.wg,probs,"<"),2,sum)+1
  t.i2 <- pmin(apply(outer(t.wg,probs,"<="),2,sum)+1,length(t.d))
  (t.d[t.i1]+t.d[t.i2])/2
}
## ==========================================================================
logst <- function(data, calib=data, threshold=NULL, mult=1)
{
  ## Purpose:   logs of data, zeros and small values treated well
  ## -------------------------------------------------------------------------
  ## Author: Werner Stahel, Date:  3 Nov 2001, 08:22
  data <- cbind(data)
  calib <- cbind(calib)
  lncol <- ncol(calib)
  ljthr <- length(threshold)>0
  if (ljthr) {
    if (is.logical(threshold)&&threshold) 
      threshold <- attr(data, "threshold")
    if (!length(threshold)%in%c(1, lncol))
      stop("!logst! argument `threshold` is inadequate")
    lthr <- rep(threshold, length=lncol)
    ljdt <- !is.na(lthr)
  } else {
    ljdt <- rep(TRUE, lncol)
    lthr <- rep(NA, lncol)
    for (lj in 1:lncol) {
      lcal <- calib[,lj]
      ldp <- lcal[lcal>0&!is.na(lcal)]
      if(length(ldp)==0) ljdt[lj] <- FALSE else {
        lq <- quantile(ldp,probs=c(0.25,0.75),na.rm=TRUE)
        if(lq[1]==lq[2]) lq[1] <- lq[2]/2
        lthr[lj] <- lc <- lq[1]^(1+mult)/lq[2]^mult
      }
    }
  }
  ## transform data
  for (lj in 1:lncol) {
    if (ljdt[lj]) {
      ldt <- data[,lj]
      lc <- lthr[lj]
      li <- which(ldt<lc)
      if (length(li))
        ldt[li] <- lc * 10^((ldt[li]-lc)/(lc*log(10)))
      data[,lj] <- log10(ldt)
  } }
  if (length(colnames(data)))
    lnmpd <- names(ljdt) <- names(lthr) <- colnames(data)  else
    lnmpd <- as.character(1:lncol)
  if (ncol(data)==1) data <- data[,1]
  attr(data,"threshold") <- unname(lthr)
  if (any(!ljdt)) {
    warning(":logst: no positive data",
            if(lncol>1) paste(" for variables ",lnmpd[!ljdt],
            ". These are not transformed") else ". No transformation")
    attr(data,"transformed") <- unname(ljdt)
  }
  data
}
## ===========================================================================
asinp <- function(x) asin(sqrt(x/100))/asin(1)
## asinperc <- asinp  ## compatibility
## ==============================================================
nainf.exclude <- function (object, ...)
  ## na.omit, modified to omit also Inf and NaN values
{
  if (is.atomic(object)) {
    i <- is.finite(object)
    if (length(dim(i))) ## matrix 
      return( object[apply(i,1,all),,drop=FALSE] )
    else return( object[i] )
  }
  ## list
  n <- length(object)
    omit <- FALSE
    vars <- seq_len(n)
    for (j in vars) {
        x <- object[[j]]
        if (!is.atomic(x))
            next
##-         x <- is.na(x)
        x <- if (is.numeric(x)) !is.finite(x) else is.na(x)
        d <- dim(x)
        if (is.null(d) || length(d) != 2)
            omit <- omit | x
        else for (ii in 1:d[2]) omit <- omit | x[, ii]
    }
  xx <- object[!omit, , drop = FALSE]
  if (any(omit > 0L)) {
        temp <- seq(omit)[omit]
        names(temp) <- attr(object, "row.names")[omit]
        attr(temp, "class") <- "exclude"
        attr(xx, "na.action") <- temp
    }
    xx
  }
## ===========================================================================
modarg <- function(arg=NULL, default) {
  if (is.null(arg)) return(default)
  if (is.null(names(arg))) { ## unnamed
    if (length(arg)>length(default)) {
      warning(":modarg: argument too long. I use default")
      return(default)
    }
    names(arg) <- names(default)[1:length(arg)]
  }
  if (any(i <- names(arg)%nin%names(default))) {
    warning(":modarg: argument has unsuitable names: ", names(arg)[i])
    arg <- arg[!i]
  }
  if (length(arg)==0) return(default)
  if (is.list(default)) arg <- as.list(arg)
  default[names(arg)] <- arg
  default
}
## ===========================================================================
## ===========================================================================
doc <- function(x) attr(x,"doc")
## ---
"doc<-" <- function(x, value)
{
  ##-- Create doc attribute or  PREpend  new doc to existing one.
  value <- as.character(value)
  attr(x, "doc") <- if (length(value)==0) NULL else
  if(value[1]=="^") value[-1] else c(value, attr(x, "doc"))
  x
}
## ---
tit <- function(x) attr(x,"tit")
## ---
"tit<-" <- function(x, value) ## ! argument must be `value`. demanded by attr
{
  attr(x, "tit") <- value
  x
}
## ---
## ===========================================================================
## additional useful functions
## ===========================================================================
dropdata <- function(data, rowid=NULL, incol="row.names", colid=NULL)
{
  ## Purpose:   drop observations from a data frame
  ## ----------------------------------------------------------------------
  ## Author: Werner Stahel
  li <- lj <- NULL
  lattr <- attributes(data)
  lattr <- lattr[is.na(match(names(lattr),
                             c("dim","dimnames","row.names","names")))]
  ln <- NROW(data)
  if (!is.null(rowid)) {
    lrn <- RNAMES(data)
    if (is.null(lrn)) lrn <- as.character(1:NROW(data))
    if (incol=="row.names")
      li <- match(as.character(rowid),lrn,nomatch=0)
    else {
      incol <- if (is.numeric(incol)) (1:ncol(data))[incol] else
      match(incol, colnames(data))
      if (is.na(incol)) stop("misspecified argument `incol`")
      li <- match(rowid,data[,incol],nomatch=0)
    }
    if (any(li==0)) warning(":dropdata: observations",
              paste(rowid[li==0],collapse=", "),"not found")
    li <- li[li>0]
    if (!is.null(li)) {
      data <- cbind(data)[-li,]
      names(li) <- lrn[li]
    }
  }
  ## drop variables
  if (!is.null(colid)) {
    lj <- match(as.character(colid),names(data),nomatch=0)
    if (any(lj==0)) warning(":dropdata: variables  ",
              paste(colid[lj==0],collapse=", "),"  not found")
    lj <- lj[lj>0]
    if (!is.null(lj)) data <- data[,-lj,drop=FALSE]
  }
  if (length(li)==0&length(lj)==0) {
      warning(":dropdata: no data to be dropped")
      return(data)
    }
  if (length(li)) {
    if (length(li)==NROW(data)) warning(":dropobs: no observations left")
    if (length(lattr$na.action))  {
      lin <- which(naresid(lattr$na.action, 1:ln%in%li))
      names(lin) <- lrn[li]
      li <- c(lattr$na.action, lin)
    }
    class(li) <- "exclude"
    lattr$na.action <- li
  }
  attributes(data) <- c(attributes(data),lattr)
  data
}
## ===================================================
last <-
function(data,n = NULL, ncol=NULL, drop=is.matrix(data))
{
  ldim <- dim(data)
  if (is.null(ldim)) {
    if (is.null(n)) n <- 1
    ldt <- length(data)
    return(data[sign(n)*((ldt-abs(n)+1):ldt)])
  }
  if (length(ldim)!=2)
    stop ("!last! not programmed for arrays of dimension >2")
  if (is.null(n)&is.null(ncol)) n <- 1
  if (is.null(n)) n <- ldim[1]
  if (is.null(ncol)) ncol <- ldim[2]
  data[sign(n)*((ldim[1]-abs(n)+1):ldim[1]),
       sign(ncol)*((ldim[2]-abs(ncol)+1):ldim[2]), drop=drop]
}
## -----------------------------------------------------------------
notna <- function(x,inf=TRUE) if (inf) x[is.finite(x)] else x[!is.na(x)]
shorten <- function(x, n=50, endstring="...")
  if (nchar(x)>n)
    paste(substring(x, 1, n-nchar(endstring)),endstring, sep="") else x
## =================================================================
## auxiliary functions
## ============================================================
RNAMES <- function(x) if (!is.null(dim(x))) row.names(x) else names(x)

is.formula <- function(object)
  length(class(object))>0 && class(object)=="formula"
nafalse <- function(x) if (is.null(x)) FALSE else ifelse(is.na(x), FALSE, x)
u.nuna <- function(x)  length(x)==0 || (is.atomic(x)&&any(is.na(x)))
u.true <- function(x) length(x)>0 && (!is.na(lx <- as.logical(x[1]))) && lx
"%nin%" <- function(x,y) !x%in%y
u.notfalse <-
  function (x) !(length(x)==1 && is.logical(x) && (!is.na(x)) && !x)
## ----------------------------------------------------------------

u.merge <- function(dd1, dd2 = NA, which=NULL, after=NULL,
                    length=NULL, names=NULL)
{
## Purpose:   merge two vectors or expand a vector by NA s
## -------------------------------------------------------------------------
## Arguments:
##   dd1      first vector or matrix or data.frame (?),
##   dd2      second vector, ...
##   which    is T for indices for which first vector is used
##   after    elements of  dd2  will be inserted after "after" in dd1
##   length   length of the result (will be expanded if necessary)
##   names    names of the result (if length is adequate)
## -------------------------------------------------------------------------
## Author: Werner Stahel, Date: 11 Mar 93, 13:50, and later
  llen <- length
  n1 <- length(dd1)
  nc1 <- ncol(dd1)
  nc2 <- ncol(dd2)
  if (length(nc1)>0) {
    n1 <- nrow(dd1)
    if (!( length(dd2)==1 || is.null(nc2) || nc2==nc1 ))
      stop("unsuitable second argument")
    }
## --- generate  which  vector for all cases
  if (length(which)==0) {
## - after  specified
      if (length(after)==0) stop("specify either  which  or  after")
      if (is.logical(after))  after <- which(after)
      wh <- rep(TRUE,n1+length(after))
      wh[after+1:length(after)] <- FALSE }
  else {
## - which  specified
    if(is.logical(which)) wh <- which
    else {
      if (length(llen)==0)  llen <- n1+length(which)
        wh <- rep(TRUE, llen)
        wh[which] <- FALSE }
  }
## --- merge
  nn <- length(wh)
  n2 <- nn-n1
  if (!(is.null(names)|length(names)==nn))
    warning("argument  names  not used (unsuitable length)")
  if (length(nc1)>0) {
    if (!(length(dd2)==1 || NROW(dd2)==n2))
      stop("unsuitable number of rows")
    rr <- matrix(NA,nn,nc1)
    rr[wh,] <- as.matrix(dd1)
    rr[!wh,] <- if (is.data.frame(dd2)) as.matrix(dd2) else dd2
##-     if (length(names)>0) row.names(rr) <- names else {
##-       if (length(lrn1 <- row.names(dd1))>0)
  }
  else {
    rr <- rep(NA,nn)
    rr[wh] <- dd1
    rr[!wh] <- dd2
    if (length(names)>0) names(rr) <- names
  }
  rr
}
## ==========================================================================
##- is.R <- function ()
##- exists("version") && !is.null(vl <- version$language) && vl == "R"

warn <- function()
  table(paste(names(lw <- warnings()),"@",substr(unlist(lw),1,10)))

getmeth <- function(fn,mt)  getS3method(as.character(substitute(fn)),
                                        as.character(substitute(mt)))

BR <- function() {browser();browser()}
DB <- function(on=TRUE) options(error=if(on) recover else NULL, warn=on)

# options(show.termeffects=TRUE)
IR <- function(condition) {
  if (condition) {
    cat("INTERRUPT: ",as.character(substitute(condition)))
    traceback()
    browser()
  }
}
## ---------------------------------------------------------------------
transferAttributes <- function(x, xbefore)
{
  lattr <- attributes(xbefore)
  latnm <- c("class", "names", "dim", "dimnames", "row.names")
  attributes(x) <-
    c(attributes(x)[latnm],lattr[setdiff(names(lattr), latnm)])
  if (is.list(x) & is.list(xbefore) && all(names(x)==names(xbefore)))
    for (lnm in names(x))
      x[[lnm]] <- transferAttributes(x[[lnm]], xbefore[[lnm]])
  x
}
## ==========================================================================
c.weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
            "Saturday", "Sunday")
c.wkd <- substring(c.weekdays,1,3)
c.months <- c("January", "February", "March", "April", "May", "June",
        "July", "August", "September", "October", "November",
        "December")
c.mon <- substring(c.months,1,3)