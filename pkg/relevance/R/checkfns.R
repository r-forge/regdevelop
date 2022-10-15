## -----------------------------------------------------------------------
check.args <- #f
##-   function(args, check=NULL, defaults=NULL, functionname)
  function(check, envir)
{ ## check arguments of a function and return them in evaluated form
  call <- sys.call(-1)
  defaults <- as.list(args(sys.function(-1)))
  lfnargnm <- names(defaults)
  defaults <- defaults[!sapply(defaults, is.symbol)]
  args <- as.list(call[-1])
  largnm <- names(args)
  li <- largnm==""
  lni <- sum(li)
  if (lni) {
    lnm <- setdiff(lfnargnm, largnm)[seq_along(lni)]
    largnm[li] <- lnm
  }
  lfname <- as.character(call[1])
  ##-   lfn <-
##-     if(length(lfname)>1)
##-       get(lfname[[1]], paste("packagbe",lfname[[2]],sep=":")) else get(lfname[[1]])
##-   defaults <- as.list(args(lfn))
  lnl <- length(args)
  lchecknm <- intersect(names(args), names(check))
  lerror <- FALSE
  lmessage <- FALSE
  for (lnm in lchecknm) {
    lvalue <- eval(args[[lnm]], envir=envir)
    args[[lnm]] <- lvalue ## !!! I will return evaluated arguments!
    lcheck <- check[[lnm]]
    ldefault <- defaults[[lnm]]
    if (length(lcheck)) {
      if (!is.list(lcheck[[1]])) lcheck <- list(lcheck)
      lnch <- length(lcheck)
      lmsg <- rep("", lnch)
      for (lj in seq_len(lnch)) {
        lch <- lcheck[[lj]]
        lfn <- get(lch[[1]])
        lmsg[lj] <- lmsgj <-
          switch(paste("v",length(lch),sep=""), v0="", v1=lfn(lvalue),
                 v2=lfn(lvalue, lch[[2]]), v3=lfn(lvalue, lch[[2]], lch[[3]]),
                 v4=lfn(lvalue, lch[[2]], lch[[3]], lch[[4]]),
                 v5=lfn(lvalue, lch[[2]], lch[[3]], lch[[4]], lch[[5]]),"")
        if (lmsgj=="") break
      }
      if (all(lmsg!="")) {
        ldf <- eval(defaults[[lnm]])
        if (missing(ldf)) ldf <- NULL
        lms <-
          paste("argument '", lnm, "' is not suitable. It should\n    ",
                paste(lmsg, collapse=" -- or \n  "),
                "\n  instead of [str():]")
        if (length(ldf)) {
          cat("\n* Warning in ", lfname, ": ", lms)
          str(lvalue)
          cat("  default will be used",
              if(length(ldf)) paste(": ",paste(ldf, collapse=", ")))
          args[lnm] <- ldf
        } else {
          cat("\n*** Error in ", lfname, ": ", lms)
          str(lvalue)
          lerror <- TRUE
        }
        lmessage <- TRUE
      }
    }
  }
  if (lmessage) cat("\n")
  if (lerror) stop("see above", call.=FALSE)
  args[lchecknm]
}
## -----------------------------------------------------------
check.color <- #f
  function(x, dummy) {
  if (is.atomic(x) && is.character(x)) {
    lpal <- palette()
    lx <- try(palette(c(x,"black")), silent=TRUE)
    ## palette asks for at least 2 colors
    palette(lpal) ## restore palette
    if (inherits(lx, "try-error"))
      return("consist of known color names")
    else return("")
  }
  else {
    if(is.atomic(x) && is.numeric(x)) {
      if(all(x>=0 )) return("") ## & x<=length(palette())
         else return("if numeric, be >=0")  ## and <=length(palette())
    }
    else {
      if (is.matrix(x)) {
        if (!any(li <- apply(x, 2, function(x) x<0 | x>255))) return("")
        return(
          paste("be a matrix with 3 rows with numbers in [0, 255]", 
                if (length(x)>1) paste("\n  columns ",paste(li, collapse=", "),
                                       " out of range")) )
      }
    }
  }
  "be a (vector of) color name(s) or an rgb matrix"
}
##---------
check.numrange <- #f
  function(x, range, na.ok=TRUE, length=NA, dim=NA) {
  if (is.function(x)) return("be numeric")
  if (!is.na(length)) 
    if (length > (lnx <- length(x)))
      return(paste("have length at least ",length))
  if (!all(is.na(dim))) {
    ldim <- dim(x)
    if (length(ldim)<2 || any(ldim<dim, na.rm=TRUE))
      return(paste("have dimension at least  c(",
                   paste(dim, collapse=", "), ")", sep=""))
  }
  if (na.ok && (u.isnull(x) || all(is.na(x))) ) return("")
  if (!(is.numeric(x)|is.logical(x))) return("be numeric")
  if ((!na.ok) && any(is.na(x))) return("not contain NAs")
  if (all(is.na(range))) return("")
  range <- ifelse(is.na(range), c(-Inf,Inf), range)
  if (!any(li <- x<range[1]|x>range[2], na.rm=TRUE)) return("")
  paste("be within [",paste(range, collapse=", "),"]",
        if (length(x)>1) paste("\n  violated for element(s) ",
                           paste(which(li), collapse=", ")))
}
check.numvalues <- #f
  function(x, values=NA, na.ok=TRUE) {
  if (na.ok && (u.isnull(x) || all(is.na(x))) ) return("")
  if (!(is.numeric(x)|is.logical(x))) return("be numeric")
  if ((!na.ok) && any(is.na(x))) return("not contain NAs")
  if (all(is.na(values))) return("")
  if (any(li <- (!is.na(x) & (x %nin% values))))
    return(paste("have values in [",
                 paste(values[1:min(5, length(values))], collapse=", "),
                 if (length(values)>5) ",...", "]",
                 if (length(x)>1) paste("\n  violated for elements ",
                                        paste(li, collapse=", "))) )
  ""
}
check.char <- #f
  function(x, values, na.ok=TRUE) {
  if (na.ok && (u.isnull(x) || all(is.na(x))) ) return("")
  if (!is.character(x)) return("be of mode character")
  if ((!na.ok) && any(is.na(x))) return("not contain NAs")
  if (all(is.na(values))) return("")
  if (!any(li <- (!is.na(x) & (x %nin% values)))) return("")
  paste("have values in [",
                 paste(values[1:min(5, length(values))], collapse=", "),
                 if (length(values)>5) ",...", "]",
                 if (length(x)>1) paste("\n  violated for elements ",
                                        paste(li, collapse=", ")))
}
check.logical <- #f
  function(x, values, na.ok=TRUE) {
  if (na.ok && (u.isnull(x) || (is.atomic(x)&&all(is.na(x)))) ) return("")
  if ((is.logical(x) | (is.numeric(x))) && !all(is.na(x)) )  return("")
  "be of mode logical (or interpretable as such)"
}
check.dataframe <- #f
  function(x, na.ok=TRUE) {
  if (is.data.frame(x)) return("")
  if (na.ok && (u.isnull(x) || all(is.na(x))) ) return("")
  "be a data.frame"
}
check.list <- #f
  function(x, na.ok=TRUE) {
  if (is.list(x))  return("")
  if (na.ok && (u.isnull(x) || all(is.na(x))) ) return("")
  "be a list"
}
check.listnum <- #f
  function(x, values=NA, na.ok=TRUE) {
  if (is.list(x)) {
    lchk <- lapply(x, function(xx) check.numvalues(xx, values, na.ok) )
    if (all(lchk=="")) return("")
    return(paste("if a list, all components must be numeric"))
  }
  "be a list"
  }
check.class <- #f
  function(x, class, na.ok=TRUE) {
  if (inherits(x, class))  return("")
  if (na.ok && (u.isnull(x) || all(is.na(x))) ) return("")
  paste("be an object of class ", paste(class, collapse="  or  "), ".")
}
check.function <- #f
  function(x, values, na.ok=TRUE) {
  if (is.function(x)) return("")
  if (na.ok && (u.isnull(x) || all(is.na(x))) ) return("")
  if (is.character(x))  {
    lfn <- try(get(x), silent=TRUE)
    if (inherits(lfn, "try-error"))
      return(paste("be a function or the name of an existing function.\n   '",
                   lfn, "' is not available.") )
    else return("")
  }
  "be a function or the name of an existing function."
}
## ----------------------------------------------------------------------
cnr <- function(range=NA, na.ok=TRUE, length=NA, dim=NA)
  list("check.numrange", range=range, na.ok=na.ok, length=length, dim=dim)
cnv <- function(values=NA) list("check.numvalues", values=values)
cch <- function(values=NA) list("check.char", values=values)
ccl <- function() list("check.color", NULL)
clg <- function(na.ok=TRUE) list("check.logical", na.ok=na.ok)
cfn <- function(na.ok=TRUE) list("check.function", na.ok=na.ok)
cdf <- function(na.ok=TRUE) list("check.dataframe", na.ok=na.ok)
cls <- function(na.ok=TRUE) list("check.list", na.ok=na.ok)
ccls <- function(class, na.ok=TRUE) list("check.class", class=class, na.ok=na.ok)
cln <- function(values=NA) list("check.listnum", values=values)
## ---------------------------------------------------------------------
