showd <- #f
  function (data, first=3, nrow.=4, ncol.=NULL, digits=getOption("digits"))
{
  ## print some rows (and columns) of a matrix or data.frame
  if (length(data)==0) {
    cat("Null object: ", str(data), "\n")
    return()
  }
##-   if (u.notfalse(i.getploption("doc"))) {
##-     if(length(ltit <- tit(data))>0) cat(ltit,"\n")
##-     if(length(ldc <- doc(data))>0) {
##-       if (length(ldc)>3) ldc <- c(ldc[1:2],paste(ldc[3], "..."))
##-       ldc[1] <- paste(" ",ldc[1], sep="")
##-       cat(paste("  ",ldc, "\n", sep=""))
##-     }
##-   }
  lldim <- length(dim(data))
  if (lldim>2) stop("!showd not yet programmed for arrays")
  if (lldim>0) cat("dim: ",dim(data),"\n") else
    if (is.factor(data)) data <- as.character(data)
  if (is.list(data)&&!is.data.frame(data)) {
    llen <- length(data)
    lnm <- i.def(names(data), as.character(1:llen))
    for (lil in seq_len(min(llen,3))) {
      cat(paste("\n[[", lnm[lil],"]]\n", sep=""))
      showd(data[[lil]])
    }
    if (llen>3) {
      if (llen>4) cat("\n  ...\n")
      cat(paste("\n[[", lnm[llen],"]]\n", sep=""))
      showd(data[[llen]])
    }
    return(invisible(NULL))
  }
  ldata <- as.data.frame(data)
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
  if (l.nr<=nrow.+first)
    l.dc <- format(ldata[,l.ic, drop=FALSE], digits=digits)  else {
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
      format(rbind(row.names(l.dc),l.dc[,1]),justify="right", digits=digits)[1,]
    l.dc <- t(l.dc)
  }
  print(l.dc,quote=FALSE, digits=digits)
  invisible(l.dc)
}
## ===========================================================================
dropdata <- #f
  function (data, rowid=NULL, incol="row.names", colid=NULL)
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
      if (is.na(incol)) stop("misspecified argument 'incol'")
      li <- match(rowid,data[,incol],nomatch=0)
    }
    if (any(li==0)) warning(":dropdata: observations  ", ## notice
              paste(rowid[li==0],collapse=", "),"  not found")
    li <- li[li>0]
    if (length(li)) {
      data <- cbind(data)[-li,]
      names(li) <- lrn[li]
    }
  }
  ## drop variables
  if (length(colid)) {
    lj <- match(as.character(colid),names(data),nomatch=0)
    if (any(lj==0)) warning(":dropdata: variables  ", ## notice
              paste(colid[lj==0],collapse=", "),"  not found")
    lj <- lj[lj>0]
    if (length(lj)) data <- data[,-lj,drop=FALSE]
  }
  if (length(li)==0&length(lj)==0) {
      warning(":dropdata: no data to be dropped")
      return(data)
    }
  if (length(li)) {
    if (length(li)==NROW(data)) warning(":dropobs: no observations left")
    if (length(lattr$na.action))  {
      lin <- which(naresid(lattr$na.action, (1:ln)%in%li))
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
sumNA <- #f
  function (object, inf=TRUE)
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
## -----------------------------------------------------------------
replaceNA <- #f
  function (x, na, inf=TRUE) {
  ff <- if (inf) function(x) !is.finite(x)  else is.na
  if (length(x)) ifelse(ff(x), na, x) else na
}
## -----------------------------------------------------------------
dropNA <- #f
  function (x, inf=TRUE)
{
  if (length(dim(x))) {
    if (is.numeric(x)&inf) x[apply(is.finite(x),1,all)]
    else x[!apply(as.matrix(is.na(x)), 1, any),] ## as.matrix needed for Surv obj
  } else if (is.numeric(x)&inf) x[is.finite(x)] else x[!is.na(x)]
}
## ---------------------------------------------------------------------------
formatNA <- #f
  function(x, na.print=" .", digits=getOption("digits"),...)
{
  if (is.data.frame(x))
    as.data.frame(lapply(x, formatNA, na.print=na.print, digits=digits),
                  row.names=row.names(x))
  else sub("NaN",na.print,sub("NA",na.print,format(x,digits=digits,...)))
}
## ===========================================================================
asinp <- #f
  structure(
    function (x) asin(sqrt(x/100))/asin(1),
    inverse = function(x) { 100* sin(x*asin(1))^2 },
    range = c(0,100), range.transformed = c(0,1)
  )
## ==========================================================================
logst <- #f
  function (data, calib=data, threshold=NULL, mult=1)
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
      stop("!logst! argument 'threshold' is inadequate")
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
      data[,lj] <- ifelse(ldt<lc, log10(lc)+(ldt-lc)/(lc*log(10)), log10(pmax(lc,ldt)))
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
last <- function (data,n = NULL, ncol=NULL, drop=is.matrix(data))
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

