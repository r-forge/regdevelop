## relevance.R
## two groups and one sample
twosamples <- #f
  onesample <- function(x, ...) UseMethod("twosamples")
## ---
twosamples.default <- #f
  function(x, y=NULL, paired=FALSE, table=NULL,
           hypothesis=0, var.equal=TRUE,
           testlevel=getOption("testlevel"),
           log=NULL, standardize=NULL, 
           rlv.threshold=getOption("rlv.threshold"), ...)
{ ## effect (group difference) and relevance
  lcheck <-
    list(x=list(cnr(na.ok=FALSE), cdf()), y=cnr(na.ok=TRUE), paired=clg(),
         table=list(cnr(dim=c(2,2)), cdf()), hypothesis=cnr(), var.equal=clg(na.ok=FALSE),
         testlevel=cnr(range=c(0.001,0.5)), log=list(clg(), cch()),
         standardize=clg(), rlv.threshold=cnr(range=c(0,Inf))
         )
  largs <- check.args(lcheck, envir=parent.frame())
  for (lnm in names(largs)) assign(lnm, largs[[lnm]])
  ## --------------
  ltlev2 <- testlevel/2
  lonegroup <- FALSE
  lse <- NA
  ltb <- FALSE
  if (length(table)) {
    table <- as.table(as.matrix(table))
    lbin <- ltb <- TRUE
    paired <- FALSE
    lonegroup <- is.na(ncol(table))
    if (lonegroup) {
      lnx <- sum(table)
      lny <- 0
    } else {
      lnx <- sum(table[1,])
      lny <- sum(table[2,])
    }
    x <- 0
  } else {    
    if (length(y)==0) {
      if (NCOL(x)==2) {
        y <- x[,2]
        x <- x[,1]
      } else {
        if (!is.atomic(x))
          stop("!twosamples! argument 'x' not suitable")
      }
    }
    if (paired) {
      if (length(x)!=length(y))
        stop("!twosamples! 'x' and 'y' must have equal length if 'paired' is TRUE")
      x <- y-x
      lonegroup <- TRUE
    }
    else if (length(y)==0) lonegroup <- TRUE
    ## quantitative or proportion?
    lbin <- length(table(c(x,y)))<=2 ## all(c(x,y)%in%c(0,1,NA))
    ## ---
    x <- x[is.finite(x)]
    lmnx <- mean(x)
    ln <- lnx <- length(x)
    lny <- 0
    lest <- NULL
  }
  ## ---
  if (lbin) { ## binary data
    lrlvth <-
      i.def(rlv.threshold["prop"],
            c(rlv.threshold, getOption("rlv.threshold")["prop"])[1])
    lx <- if (ltb) table[2] else sum(x)
    if (lonegroup) { ## one proportion
      if (paired) x <- (x[x!=0]+1)/2 ## McNemar
      lt <- binom.test(lx,lnx, conf.level=1-testlevel)
      lest <- lt$estimate
      ## !!! se
      leff <- unname(qlogis(lest))
      leffci <- qlogis(lt$conf.int) ## c("ci.low","ci.up") ))
      lar <-
        qlogis( (qintpol(c(ltlev2, 1-ltlev2), lnx, plogis(hypothesis))+0:1) / lnx )
##                      c("accreg.low","accreg.up") ))
      lsg <- (leff-hypothesis)/abs(lar-hypothesis)
      lsig <- ifelse(leff<0, lsg[1], lsg[2])
      lv <- lest*(1-lest)
      effname <- paste("log odds", if(paired) "of changes")
      method <- "One proportion, binomial inference"
    } else { ## two proportions
      if (ltb) {
        if (any(dim(table)!=c(2,2)))
          stop("!twosamples! argument 'table' not suitable")
        } else {
          y <- y[is.finite(y)]
          lmny <- mean(y)
          lny <- length(y)
          table <- table(x,y)
        }
      lt <- fisher.test(table, conf.int=TRUE, conf.level=1-testlevel)
      lest <- table[,2]/(table[,1]+table[,2]) ## c(mean(x), mean(y))
      ## !!! se
      leff <- unname(log(lt$estimate))
      leffci <- log(lt$conf.int) ## c("ci.low","ci.up") ))
      lse <- diff(leffci)/(2*qnorm(1-ltlev2))
      lsig <- leff/(leff-ifelse(leff>0, leffci[1], leffci[2]))
      effname <- "log odds ratio"
      method <- "Two proportions, Fisher's exact inference"
    }
    lrlvci <- c(leff, leffci)/lrlvth
    ltst <- unname(lt$teststatistic)
    lpv <- lt$p.value
    ltype <- "proportion"
    lsigth <- NA
    ldf <- NULL
    lv <- NA
  } else { ## ----------- quantitative data
    log <- i.def(log, "", "log", "")
    llogrescale <- 1
    if(log=="log10") llogrescale <- 1/log(10)
    llog <- substr(log,1,3)=="log"
    lirl <- ifelse(llog,"rel","stand")
    lrlvth <-
      i.def(rlv.threshold[lirl],
            c(rlv.threshold, getOption("rlv.threshold")[lirl],
            relevance.options[["rlv.threshold"]][lirl])[1])
    lssx <- sum((x-lmnx)^2)
    lpq <- 1-ltlev2
    if (lonegroup) { ## one quantitative sample
      leff <- lmnx
      ldf <- lnx-1
      lv <- lssx/ldf
      lny <- 0
      effname <- paste("mean", if(paired) "of differences")
      method <- "One Sample t inference"
      lest <- c(mean=lmnx, se=sqrt(lv/lnx))
    } else { # two quantitative samples
      y <- y[is.finite(y)]
      lny <- length(y)
      ln <- lnx+lny
      lmny <- mean(y)
      leff <- lmny-lmnx
      lssy <- sum((y-lmny)^2)
      lsex2 <- lssx/(lnx-1)/lnx
      lsey2 <- lssy/(lny-1)/lny
      if (var.equal) {
        lv <- ln*(1/lnx+1/lny)*(lssx+lssy)/(ln-2)
        ldf <- ln-2
        method <- "Two Sample t inference, equal variances assumed"
      } else { ## welch
        lv <- ln*(lssx/(lnx-1)/lnx+lssy/(lny-1)/lny)
        ldf <- (lsex2+lsey2)^2/(lsex2^2/(lnx - 1) + lsey2^2/(lny - 1))
        method <- "Two Sample t inference, unequal variances (Welch)"
      }
      effname <- "difference of means"
      lest <- cbind(mean=c(lmnx,lmny), se=sqrt(c(lsex2, lsey2)))
    }
    lse <- sqrt(lv/ln)
    ltst <- leff/lse
    lq <- qt(lpq, ldf)
    lciwid <- lq*lse
    leffci <- leff + c(-1,1)*lciwid
    lpv <- 2*pt(-abs(ltst), ldf)
    ## 
    lsc <- 1
    ltype <- "raw"
    if (llog) {
      lsc <- llogrescale
      ltype <- "relative"
    } else if(u.notfalse(standardize)) {
      lsc <- sqrt(lv)
      ltype <- "standardized"
    }
    lrlvci <- c(leff,leffci)/lsc/lrlvth
    lsig <- leff/lciwid
    lsigth <- c((lrlvci[1]-1)*2/diff(lrlvci[2:3]))
  }
  if (leff<0) lrlvci <- -lrlvci[c(1,3,2)]
  rr <- structure(
    c(effect=leff, se=lse, teststatistic=ltst, p.value=lpv, Sig0=lsig,
      ciLow=leffci[1], ciUp=leffci[2],
      Rle=lrlvci[1], Rls=lrlvci[2], Rlp=lrlvci[3],
      Sigth=lsigth,
      df = ldf, scatter=sqrt(lv)),
    class = c("inference", "twosamples"), type="simple",
    method = method, effectname = effname,
    hypothesis = hypothesis,
    n = c(lnx, lny), teststatistic = ltst, ## V = lv, df = ldf,
    estimate = lest, 
    data = if (ltb) table else {if (lonegroup) x else list(x=x, y=y)},
    rlv.threshold = lrlvth,
    rlv.type = ltype
  )
  attr(rr, "rlvclass") <- rlvClass(rr)
  rr
} # end twosamples.default
## ============================================================================
twosamples.formula <- #f
  function (x, data=NULL, subset, na.action, log=NULL, ...)
{ ## adapted from t.test.formula
  lcheck <-
    list(data=cdf(), na.action=cfn())
  largs <- check.args(lcheck, envir=parent.frame())
  for (lnm in names(largs)) assign(lnm, largs[[lnm]])
  ## --------------
  if (missing(x) || (length(x) != 3L))
    stop("!twosamples! 'formula' must have left and right term")
  lfy <- x[[2]]
  llog <- 
    if(length(lfy)>1) 
      if(as.character(lfy[1])=="log") "log"
      else if(as.character(lfy[1])%in%c("log10", "logst")) "log10"
  llog <- i.def(log, if(is.null(llog)) "" else llog)
  ## ---
  oneSampleOrPaired <- FALSE
  if (length(attr(terms(x[-2L]), "term.labels")) != 1L)
    if (x[[3]] == 1L)
      oneSampleOrPaired <- TRUE
    else stop("!twosamples! 'formula' incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  names(m)[2] <- "formula"
  mf <- eval(m, parent.frame())
  ## DNAME <- paste(paste("`",names(mf),"'",sep=""), collapse = " by ")
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  if (!oneSampleOrPaired) { ## two indep. samples
    g <- factor(mf[[-response]])
    if (nlevels(g) != 2L)
      stop("grouping factor must have exactly 2 levels")
    rr <- do.call("twosamples",
                  c(setNames(split(mf[[response]], g), c("x", "y")),
                    list(paired=FALSE, log=llog, ...)))
##    rr <- twosamples.default(DATA$x, DATA$y, paired=FALSE, log=llog, ...)
    names(attr(rr, "n")) <- levels(g)
  } else { ## one sample
    respVar <- mf[[response]]
    if (inherits(respVar, "Pair")) {
      rr <- do.call("twosamples",
                    list(x = respVar[, 1], y = respVar[, 2],
                         paired = TRUE, log=llog, ...))
    } else {
      rr <- do.call("twosamples",
                    list(x = respVar, paired = FALSE, log=llog, ...))
    }
  }
  ##  attr(rr, "data.name") <- DNAME
  ldn <- substitute(data)
  structure(rr, formula=x, data.name=if (is.name(ldn)) format.default(ldn))
}
## -------------------------------------------------------------------------
twosamples.table <- #f
  function(x, ...) twosamples.default(table=x, ...)
## ========================================================================
correlation <- #f
  function(x, y = NULL, method = c("pearson", "spearman"), ## , "kendall"
           hypothesis = 0, testlevel=getOption("testlevel"),
           rlv.threshold=getOption("rlv.threshold"), ...)
{ ## ---
  lcheck <-
    list(x=cnr(na.ok=FALSE), y=cnr(na.ok=TRUE), hypothesis=cnr(c(-1,1)), 
         testlevel=cnr(range=c(0.001,0.5)),
         method=cch(c("pearson", "spearman")), ## , "kendall"
         rlv.threshold=cnr(range=c(0,Inf))
         )
##-   lcall <- match.call(expand.dots = FALSE)
##-   lcall$... <- NULL
  largs <- check.args(lcheck, envir=parent.frame())
  for (lnm in names(largs)) assign(lnm, largs[[lnm]])
  ## --------------
  if (length(y)) {
    if (length(x)!=length(y))
      stop("!correlation! 'x' and 'y' must have the same length")
    x <- cbind(x,y)
  }
  if (NCOL(x)!=2) 
    stop("!correlation! argument 'x' not suitable")
  x <- stats::na.omit(x)
  ln <- nrow(x)
  if (ln<=3)
    stop("!correlation! not enough observations")
  ## ---------  
  testlevel = i.def(testlevel, getOption("testlevel"))
  lrlvth <-
    i.def(rlv.threshold["corr"],
          c(rlv.threshold, getOption("rlv.threshold")["corr"],
            relevance.options[["rlv.threshold"]]["corr"])[1])
  lmethod <- i.def(method, "pearson")
  ## ---
  lt <- cor.test(x[,1], x[,2], method=lmethod[1], conf.level=1-testlevel, ...)
  lest <- unname(lt$estimate) 
  lci <- lt$conf.int
  lci <- if (length(lci)) atanh(lci)
         else atanh(lest) + c(-1,1)*qnorm(1-testlevel/2)/sqrt(ln-3)
  ## !!! this may not be quite adequate for nonparametric correlation ...
  ## ---
  leffci <- c(atanh(lest),lci)-atanh(hypothesis)
  lrlvci <- leffci/lrlvth
  lrlvwid <- lrlvci[1]-ifelse(lrlvci[1]>0, lrlvci[2], lrlvci[3])
  lsig <- lrlvci[1]/lrlvwid
  lsigth <- (lrlvci[1]-1)/lrlvwid
  leffnm <- "correlation, z-transformed"
  lmeth <- paste("Correlation --",
                  switch(lmethod[1],
                         pearson="Pearson product moment c.",
                         spearman="Spearman's rank c.",
                         kendall="Kendall's nonparametric c.") )
  structure(
    c(effect = c(correlation=atanh(lest)),
      teststatistic=lt$teststatistic, p.value=lt$p.value, Sig0=lsig,
      ciLow=leffci[2], ciUp=leffci[3],
      Rle=lrlvci[1], Rls=lrlvci[2], Rlp=lrlvci[3], Sigth=lsigth),
    class = c("inference"), type="simple",
    method = lmeth, effectname = leffnm, hypothesis = hypothesis,
    n = length(x)-sumNA(x),
    estimate = c(lest, ciLow=lt$conf.int[1], ciUp=lt$conf.int[2]),
    data = x, rlv.threshold = lrlvth, rlv.type="correlation"
  )
} ## end correlation
## ========================================================================
inference <- #f
  function (object=NULL, estimate=NULL, teststatistic=NULL,
            se=NA, n=NULL, df=NULL, stcoef=TRUE, 
            rlv=TRUE, rlv.threshold=getOption("rlv.threshold"),
            testlevel = getOption("testlevel"), ...)
{ ## for a coefficients table,
  ## calculate confidence interval, significance and relevance
  lcheck <-
    list(object=list(cnr(),cls()), estimate=cnr(),
         teststatistic=cnr(), se=cnr(), n=cnr(range=c(2,Inf)),
         df=cnr(range=c(1,Inf)), stcoef=list(clg(),cnr()),
         rlv=clg(), rlv.trheshold=cnr(range=c(0,Inf)),
         testlevel=cnr(range=c(0.001,0.5))
         )
  largs <- check.args(lcheck, envir=parent.frame())
  for (lnm in names(largs)) assign(lnm, largs[[lnm]])
  ## --------------------------------
  if (inherits(object, "inference"))
    warning(":inference: The first argument is already an 'inference' object")
  if (inherits(object, relevance.modelclasses)) {
    lest <- getcoeftable(object)
    lsum <- summary(object)
    lcl <- object$call
    ldn <- lcl$data
    if(is.symbol(ldn)) ldn <- as.character(ldn)
##-     if (length(ldf)==0)
##-       ldf <-
##-         if (class(object)[1]%in%c("rlm", "coxph"))
##-           length(object$residuals)-length(object$coef)
##-         else df.residual(object)
    ## ldfm <- summary(object)$df[1] ## ok for lm, glm, survreg, ...
    ltt <- termtable(object, lsum, rlv=rlv, ...)
    ## special for polr
    lic <- NULL
    if (inherits(object,"summary.polr")) {
      lcf <- coefficients(lsum)
      lic <- lcf[(lsum$pc+1):nrow(lcf),1:2]
    }
    rr <-
      structure(
        list(summary = lsum, termtable = ltt, 
             termeffects = termeffects(object, rlv=rlv, ...),
             intercepts = lic
             ## , deviancetable = NULL  !!!
             ), 
        class=c("inference", "list"), type = "model",
        method=as.character(lcl[1]), formula=lcl$formula, data.name=ldn,
        rlv.threshold=attr(ltt, "rlv.threshold"))
    ## --- standardized coefficients
    if (!u.notfalse(stcoef)) return (rr)
    ##    warning(":ciSigRlv: no standardized coefficients given. No relevances")
    if (length(stcoef)==0||u.true(stcoef)) {
      if (length(object)==0) {
        warning(":inference: argument is not a fitted model. No relevances")
        return (rr)
      }
      if (inherits(object, "nls")) {
        warning(":inference: cannot calculate standardized coefficients for 'nls' objects.")
        return(rr)
      }
      lfac <- getcoeffactor(object)
      ##   lcls <- attr(lfac, "fitclass")
      stcoef <- lest[,1]*lfac[row.names(lest)]
    }
    browser()
    if (length(stcoef)!=NROW(lest)) {
      warning(":inference: argument 'stcoef' not suitable. No relevances")
      return (rr)
    }
    lfac <- stcoef/lest
    lstci <- cbind(eff=stcoef, effLow=lfac*lci[,1], effUp=lfac*lci[,2])
    rr <- cbind(rr, lstci)
    ## ---
    return(rr)
  } ## end model
  ## --------------------------------------------------
  if (length(object) && length(dim(object))==0) {
    if(inherits(object, "htest"))
      stop("!inference! cannot cope with test results.",
           "Call inference on data directly!")
    ## object <-as.data.frame(rbind(object))
  }
  ldt <- i.getIfrData(object, estimate=estimate, 
                      teststatistic=teststatistic, se=se, n=n, df=df)
  lest <- ldt$estimate
  lse <- ldt$se ## NA is ok
  if (length(lse)==0) lse <- attr(object, "se")
  ltst <- i.def(ldt$teststatistic, lest/lse)
  ln <- ldt$n
  ldf <- i.def(ldt$df, attr(object, "df"))  ## available if estimate is generated by getcoeftable
  ldf <- i.def(ldf, ln-1)
  ldfm <- 1 ## potentially modified in some cases
  ln <- i.def(ln, ldf+ldfm) 
  if (length(ln)==0 || any(is.na(ln)))
    stop("!inference! either 'n' or 'df' must be available")
  if (length(lse)==0 || all(is.na(lse))) lse <- lest/ltst
  if (length(lse)==0) {
    warning(":inference: no standard error available -> NA")
    lse <- NA
  }
##-   df <- i.def(ldf, ln-1, valuefalse=NA)
##-   if (i.def(df,0)==0) {
##-     warning(":inference: no finite standard errors")
##-     return(cbind(object))
##-   }
  ## ---
##-   if (!(is.atomic(object)||length(dim(object))==2))
##-     stop("!inference! first argument not suitable")
##-   if (is.null(se))
##-     if (NCOL(object)>1) {
##-       se <- object[,2]
##-     }
##-   if (length(dim(object)))
##-     object <- structure(object[,1], names=row.names(object))
  ltq <- qt(1-testlevel/2, ldf) ## if (is.finite(ldf)) qt(1-testlevel/2, ldf) else qnorm(1-testlevel/2)
  if(length(ldf)==1) ltq <- rep(ltq, length(lest))
  lsc <- lse*sqrt(ln)
  lest <- i.def(lest, ltst*lse)
  lci0 <- outer(ltq, c(estimate=0, ciLow=-1, ciUp=1)) ## matrix
##  if(length(ltq)==1) lci0 <- lci0[rep(1,length(lse)),]
  lci <- lest+lci0*lse
  ## ltst <- lest/lse
  leff <- ltst/sqrt(ln)
  leffci <- structure(leff+lci0/sqrt(ln),
                      dimnames=list(NULL,c("effect", "effLow", "effUp")))
  lsgf <- ltst/ltq
  lpv <- 2*pt(-abs(ltst), ldf)
  rr <- data.frame(lci, leffci, se=lse, 
                   teststatistic=ltst, p.value=lpv, Sig0=lsgf,
                   n=ln, scatter=lsc)
  ## --- relevance
  if (!u.notfalse(rlv)) return(rr)
  if (length(rlv.threshold)>1) rlv.threshold <- rlv.threshold["coef"]
  if (is.na(rlv.threshold)) {
    warning(":inference: argument 'rlv.threshold' not suitable. No relevances")
    return (rr)
  }
  lrlv <- leffci/rlv.threshold
  li <- which(leffci[,1]<0)
  if (length(li)) lrlv[li,] <- - lrlv[li,c(1,3,2)]
  colnames(lrlv) <- c("Rle","Rls","Rlp")
  lrlvclass <- rlvClass(lrlv[,"Rle"], lrlv[,c("Rls","Rlp")])
  structure(cbind(rr, lrlv, rlvclass=lrlvclass),
            row.names=row.names(ldt),
            class=c("inference", "data.frame"), type = "simple")
}
## ==========================================================================
termtable <- #f
  function (object, summary=summary(object), testtype=NULL, r2x = TRUE,
            rlv = TRUE, rlv.threshold = getOption("rlv.threshold"), 
            testlevel = getOption("testlevel"), ...)
{
  ## Purpose:  generate term table for various models
  ## --------------------------------------------------------
  lcheck <-
    list(object=cls(), summary=cls(), testtype=cch(), r2x=clg(na.ok=FALSE),
         rlv=clg(), rlv.threshold=cnr(range=c(0,Inf)),
         testlevel=cnr(range=c(0.001,0.5))
         )
##-   lcall <- match.call(expand.dots = FALSE)
##-   lcall$... <- NULL
  largs <- check.args(lcheck, envir=parent.frame())
  for (lnm in names(largs)) assign(lnm, largs[[lnm]])
  ## --------------------------------
  ## thresholds
  ltlev1 <- 1-testlevel
  ## lrlthrl <- rlv.threshold[["rel"]]
  lrlthcf <- rlv.threshold[["coef"]]
  lrlthdr <- rlv.threshold[["drop"]]
  lrlthpr <- rlv.threshold[["pred"]]
  ## --- scatter and threshold for standardized coefficient
  ## lscatter <- c(object$scatter, summary$scatter, 1)[1]
  lf.intercept <- function(object)
    length(ln <- names(object$coefficients))&&ln[1]=="(Intercept)" ## !!! polr and other models?
  lfamily <- if (is.character(lfm <- object$family)) lfm else lfm$family
  ldist <- object$dist
  lmethod <- paste(c(setdiff(class(object),"regr")[1],
                     if (length(lfamily)) c(" ; family = ", lfamily),
                     if (length(ldist)) c(" ; distribution = ", ldist),
                     " :  Drop-term inference"),
                   collapse="")
  ldname <- attr(object, "data.name")
  if (length(ldname)==0) ldname <- object$call$data
  ldname <- if (is.language(ldname)) as.character(ldname)
            else {
              if (!is.character(ldname)) ldname
            }
  ## if ((inherits(object, "glm") &&
  ##      lfamily %in% c("poisson","quasipoisson")) ||
  ##     (inherits(object, "survreg")&&
  ##      ldist %in% c("weibull", "exponential", "lognormal"))
  ##     )  lrlthcf <- lrlthrl
  ## --- testtype
  if (length(testtype)==0) {
    testtype <- "LRT"
    if (inherits(object, c("lm","lmrob"))) testtype <- "F"
    if (inherits(object, "glm")) {
      testtype <-
        if (lfamily %in% c("quasibinomial","quasipoisson")) "F" else "LRT"
    }
    if (inherits(object, c("survreg", "polr"))) testtype <- "Chisq"
  }
  ## ---
  lterms <- terms(object)
  ldfres <- df.residual(object)
  if (class(object)[1]%in%c("rlm", "coxph"))
    ldfres <- length(object$residuals)-length(object$coef)
  if(length(attr(lterms,"term.labels"))==0 | ldfres<1)
    return(data.frame(
      coef=i.def(object$coefficients,NA), df = NA, se=NA,
      teststatistic=NA, p.value=NA, Sig0=NA, ciLow=NA, ciUp=NA,
      stcoef=NA, stciLow = NA, stciUp = NA, R2x=NA,
      stringsAsFactors=FALSE)
      )
  ## --- coefficients
  lcoef <- object$coefficients
  lcft <- getcoeftable(object)
  colnames(lcft) <-
    c("estimate","se","teststatistic","p.value")[1:ncol(lcft)]
  lcoeftab <-
    inference(lcft, rlv=rlv, rlv.threshold=lrlthcf)
  names(lcoeftab)[1] <- "coef"
  ## --- drop1
  ldr1 <-
    if (inherits(object, c("lm","lmrob"))&&!inherits(object, "glm")) {
      try(drop1Wald(object, test=testtype, scope=lterms), silent=TRUE)
    } else {
      try(i.drop1(object, scope=lterms, test=testtype), silent=TRUE)
    }
  if (inherits(ldr1, "try-error")) {
    warning(":termtable: drop1 did not work. I return the coefficient table")
    lii <- match(c("Rle","Rls","Rlp"), names(lcoeftab), nomatch=0)
    names(lcoeftab)[lii] <- c("coefRle","coefRls","coefRlp")[lii!=0]
    return(
      structure(cbind(coef=lcoeftab[,1,drop=FALSE],df=1,lcoeftab[,-1,drop=FALSE]),
                class=c("inference", "data.frame"), type="terms",
                formula=formula(object), data.name=ldname
                ))
  }
  ldr1 <- ldr1[-1,]
  ldr1$RSS <- NULL # same ncol for lm and glm
  ldf <- ldr1[,1]
  if (inherits(object,"rlm"))  ldr1[,4] <- ldr1[,2]/ldf
  ## -- critical value for test
  ltstq <- if (testtype=="F") qf(ltlev1,c(1,pmax(1,ldf)),ldfres) else {
    if (testtype%in%c("Chisq", "LRT")) qchisq(ltlev1,c(1,pmax(1,ldf))) else NA }
  ltstq[which(ldf==0)+1] <- NA
  ltstq1 <- sqrt(ltstq[1]) ## 1 degree of freedom
  ltstq <- ltstq[-1]
##-   if (inherits(object,"mlm")||inherits(object,"manova"))
  ##-     return(list(termtable=ldr1))  ## !!! needs much more
  ## ---------------------
  ## model.matrix
  lmmt <- object[["x"]]
  if (length(lmmt)==0)  lmmt <- model.matrix(object)
  lasg <- attr(lmmt,"assign")[!is.na(lcoef)]
##  if (class(object)[1] %in% c("polr")) lasg <- lasg[-1] ## ,"coxph"
  ## terms without factor involvement
  lfactors <- attr(lterms,"factors")
  lvcont <- !attr(lterms,"dataClasses")[row.names(lfactors)] %in%
    c("numeric","logical") ## [...] excludes .weights. and possibly others
  ## terms only containing continuous variables
  lcont <- which( lvcont %*% lfactors ==0 )
  ## -----------------------------------
  ## --- r2x
  lr2 <- NA
  if (r2x) 
  {
    if (inherits(object, c("polr", "coxph")))
      warning("!termtable! No R2x for such models")
    else {
      lvift <-     ## lterms: n of levels for each term
        ##        if (u.debug()) vif.regr(object, mmat=lmmt) else
        try(vif.regr(object, mmat=lmmt), silent=TRUE)
      if (inherits(lvift, "try-error") || length(lvift)==0) {
        warning(":termtable: error in the calculation of R2x")
        lvif <- NA
      } else lvif <- lvift[,3]^2
      lr2 <- 1-1/lvif
    }
  }
  ## -----------------------------------
  ## --- prepare table
  lpvcol <- pmatch("Pr(",names(ldr1), nomatch=ncol(ldr1))
  lpv <- ldr1[,lpvcol]
  ldf <- ldr1[,1]
  ## !!!
  lcont <- which(ldf==1)
  lnobs1 <- ldfres+sum(ldf)-lf.intercept(object)
  ## drop effect relevance
  ltst <- ldr1[, ifelse(inherits(object, "polr"), 3, 4)]
  ldrncci <- rbind(confintF(ltst, ldf, ldfres, testlevel))
  ldreff2 <- cbind(ltst*ldf, ldrncci)/lnobs1
  ldrrl <- cbind(NA,NA,NA, sqrt(ldreff2)/lrlthdr,
                 pmax(0.5*log((ldfres+ldreff2*lnobs1)/(ldfres+ldf))/lrlthpr, 0))
  dimnames(ldrrl) <-
    list(NULL, c(t(outer(c("coef","drop","pred"),c("Rle","Rls","Rlp"),paste, sep=""))))
  ltst <- ldr1[,lpvcol-1]
  ## table, filled partially
  ltb <- data.frame(coef=NA, ldr1[,1,drop=FALSE], ## keep name if only 1 coef
                    se=NA, teststatistic=ltst, p.value=lpv,
                    Sig0=sqrt(pmax(0,ltst)/ltstq), ciLow=NA, ciUp=NA,
                    stcoef=NA, stciLow=NA, stciUp=NA, R2x=lr2, ldrrl,
                    stringsAsFactors=FALSE)
  names(ltb)[2] <- "df"
  ## intercept
  ljint <- "(Intercept)"==names(lcoef)[1]
  if (ljint) {
    ltb <- rbind("(Intercept)"=ltb[1,],ltb)
    ltb[1,] <- NA
    ltb[1,"df"] <- 1
    ltstq <- c(ltstq1, ltstq)
    lcont <- c(0, lcont)
  } else if (!inherits(object, c("coxph", "polr")))
    warning(":termtable: No intercept. Statistics are difficult to interpret.")
  lcont1 <- lcont+ljint  # row number in dr1
  ## --- coefficients and statistics for terms with 1 df
  if (length(lcont)) { ## lcont refers to assign
    ## ltlb <- dimnames(ltb)[[1]]
    ## lclb <- ltlb[lcont1] ## lcont1 is the row in the coef table of summary(object)
    ljc <- match(lcont,lasg) # index of coefs for cont variables
##    lj <- c("coef","se","teststatistic","ciLow","ciUp","stcoef", "stciLow","stciUp",
    lj <- c("coef","se","teststatistic","ciLow","ciUp","effect", "effLow","effUp",
            "coefRle","coefRls","coefRlp")
    ljj <- sub("coefRl","Rl",lj)
    ltb[lcont1,lj] <- lcoeftab[ljc,ljj]
    ltb[lcont1,"Sig0"] <- sign(ltb[lcont1,"coef"])*ltb[lcont1,"Sig0"]
  }
  structure(ltb, class=c("termtable", "data.frame"), type="terms",
            testtype=testtype, method=lmethod, 
            fitclass=class(object), family=lfamily, dist=ldist,
            formula=formula(object), data.name=ldname,
            rlv.threshold=rlv.threshold[c("coef","drop","pred")])
}
## end termtable
## --------------------------------------------------------------------
i.drop1 <- #F
  function (object, scope=drop.scope(object), scatter = 0, test = NULL, k = 2,
           sorted = FALSE, ...)
{
  ## Purpose:    drop1/add1 for regr objects
  ## ----------------------------------------------------------------------
  lfam <- i.def(object$distrname, "gaussian")
  if (is.null(test))
    test <- if (inherits(object, c("lm","roblm"))||
        ((lfam=="binomial"|lfam=="poisson")&&object$dispersion>1)) {
          if (inherits(object,"mlm")) "Wilks" else "F" }
    else "Chisq"
  if (length(scope)==0) {  ## || !is.formula(scope) ## drop.scope is character
    warning(":drop1/add1.regr: no valid scope")
    return(data.frame(Df = NA, "Sum of Sq" = NA, RSS =NA, AIC = NA,
                      row.names = "<none>") )
  }
  class(object) <- setdiff(class(object), "regr")
  fcall <- object$funcall
  if (!is.null(fcall)) object$call <- fcall
##-     if (class(object)[1]%in%c("lmrob")) ## to be expanded
##-        drop1Wald(object, test="F", ...) else {
  ldata <- object$model
  if (is.null(ldata)) ldata <- eval(object$call$data)[,all.vars(formula(object))]
  if (is.null(ldata)) stop("!i.drop1! no data found ")
  ## all predictors must get the same missing observations
  object$call$data <- na.omit(ldata)
  dr1 <- drop1(object, scope=scope, scatter=scatter, test=test, k=k, ...)
  if (sorted) 
    if (0!=(lsrt <- match(c("AIC","p.value"),colnames(dr1), nomatch=0)))
      dr1 <- dr1[order(dr1[, lsrt[1]]), ]
  dr1
}
## ===========================================================
termeffects <- #f
  function (object, se = 2, df = df.residual(object),
            rlv = TRUE, rlv.threshold=getOption("rlv.threshold"), ...)
    ## --------------------------------------------------------------
{
  lcheck <-
    list(object=cls(), se=cnr(), df=cnr(),
         rlv=clg(), rlv.threshold=cnr(range=c(0,Inf))
         )
##-   lcall <- match.call(expand.dots = FALSE)
##-   lcall$... <- NULL
  largs <- check.args(lcheck, envir=parent.frame())
  for (lnm in names(largs)) assign(lnm, largs[[lnm]])
  ## --------------------------------
  if (is.atomic(object)||is.null(terms(object)))
      stop("!termeffects! inadequate first argument")
 ##  xl <- object$xlevels
  Terms <- delete.response(terms(object))
  int <- attr(Terms, "intercept")
  facs <- attr(Terms, "factors")
  if (length(facs)==0) {
    message(".termeffects. No terms, no termeffects")
    return(NULL)
  }
  lfamily <- if (is.character(lfm <- object$family)) lfm else lfm$family
  lmethod <- paste(c(class(object),
                     if (length(lfamily)) c(" ; family = ", lfamily),
                     if (length(ldist <- object$dist)) c(" ; distribution = ", ldist),
                     " :  Term effects"),
                   collapse="")
  tl <- attr(Terms, "term.labels")
  dcl <- attr(Terms,"dataClasses")[-1]
  ## result already available?
  allc <- object$termeffects
  if ((!is.null(allc))&&length(allc)==length(tl)&&
      (is.matrix(allc[[length(allc)]])|!se)) return(allc) ## !!! check!
  ## ---
  ##  if (rlv) lscatter <- getscalepar(object)
  mf <- object$model  ##! d.c used all.vars
  if (is.null(mf)) {
    object$call$model <- object$call$x <- object$call$envir <- NULL
    mf <- model.frame(object)
  }
  xtnm <- dimnames(facs)[[1]]  ## names  ##! replaces vars
  xtlv <- lapply(mf[,xtnm, drop=FALSE],function(x) levels(x)) ## levels
  lcontr <- object$contrasts
  imat <- which(substr(dcl,1,7)=="nmatrix") ## resulting from bs()
  if (length(imat)) {
    xtlv[imat] <-
        lapply(as.list(dcl[imat]),
               function(x) as.character(1:as.numeric(substr(x,9,12))))
    ##    lcontr <- c(lcontr, structure(rep(contr.id,length(tl)), names=tl)[imat])
    lctr <- list()
    for (li in imat) lctr <- c(lctr, list(diag(length(xtlv[[li]]))))
    names(lctr) <- names(dcl)[imat]
    lcontr <- c(lcontr, lctr)
  }
  xtnl <- pmax(sapply(xtlv,length),1) ## number of levels
  termnl <- apply(facs, 2L, function(x) prod(xtnl[x > 0])) ##! lterms
  nl <- sum(termnl)
## --- df.dummy: data frame of simple terms
  args <- setNames(vector("list", length(xtnm)), xtnm)
  for (i in xtnm)
    args[[i]] <- if (xtnl[[i]] == 1)  rep.int(1, nl)    else
      factor(rep.int(xtlv[[i]][1L], nl), levels = xtlv[[i]])
  df.dummy <- as.data.frame(args) # do.call("data.frame", args)
  names(df.dummy) <- xtnm
  ## rnn: names of rows
  pos <- 0
  rn <- rep.int(tl, termnl)
  rnn <- rep.int("", nl)
  ## fill df.dummy
  for (j in tl) {
    i <- unlist(xtnm[facs[, j] > 0])
    ifac <- i[xtnl[i] > 1]
    if (length(ifac) == 0L) {
      rnn[pos + 1] <- j
    }
    else if (length(ifac) == 1L) {
      df.dummy[pos + 1L:termnl[j], ifac] <- xtlv[[ifac]]
      rnn[pos + 1L:termnl[j]] <- as.character(xtlv[[ifac]])
    }
    else {
      tmp <- expand.grid(xtlv[ifac])
      df.dummy[pos + 1L:termnl[j], ifac] <- tmp
      rnn[pos + 1L:termnl[j]] <-
        apply(as.matrix(tmp), 1L, function(x) paste(x, collapse = ":"))
    }
    pos <- pos + termnl[j]
  }
  ## attributes
  attr(df.dummy,"terms") <- attr(mf,"terms")
  lci <- sapply(df.dummy,is.factor)
  lcontr <- lcontr[names(lci)[lci]] ## factors with 1 level have disappeared (?)
  if (inherits(object, "polr")) {
    attr(Terms, "intercept") <- 1
    mm <- model.matrix(Terms, df.dummy, contrasts.arg=lcontr, xlev=xtlv)
    asgn <- attr(mm, "assign")[-1]
    mm <- mm[,-1]
  } else {
    mm <- model.matrix(Terms, df.dummy, contrasts.arg=lcontr, xlev=xtlv)
    asgn <- attr(mm, "assign")
  }
  if (anyNA(mm)) {
    warning("some terms will have NAs due to the limits of the method")
    mm[is.na(mm)] <- NA
  }
  ## calculate dummy coefs
  coef <- object$coefficients ##!!! cf <-
##-   if (!use.na)
  ##-     coef[is.na(coef)] <- 0
  names(asgn) <- colnames(mm)
##-   lnna <- is.finite(coef)
##-   if (any(!lnna)){
##-     coef <- coef[lnna]
##-     mm <- mm[,lnna]
##-     asgn <- asgn[lnna]
##-   }
  if (se) {
    cov <- vcov(object)
    if (is.null(cov)) {
      warning(":termeffects: no covariance matrix of coefficients found.",
              " Returning coefficients only")
      se <- FALSE
    } else if (inherits(object, "polr"))
      cov <- cov[1:length(coef),1:length(coef)]
  }
##-   licf <- pmatch(colnames(mm), names(coef))
  licf <- pmatch(names(coef), colnames(mm))
  lasgn <- asgn[licf]
  mm <- mm[,licf,drop=FALSE]
##  asgn <- asgn[names(coef)] ## !!!
  res <- setNames(vector("list", length(tl)), tl)
  ljfail <- NULL
  for (j in seq_along(tl)) {
    mmr <- rn == tl[j]  ## rows corresponding to the term
    mmc <- lasgn==j ## & !is.na(coef)
##-     lcf <- coef[licf[mmc]]
    lcf <- coef[mmc]
##    mmc <- names(asgn)[asgn == j & !is.na(coef)]  ## lcols (logical fails for polr, vcov() too large) !!! was  which
    ##-     mmpart <- mm[mmr, mmc, drop=FALSE]
    if (all(is.finite(lcf))) {
      mmpart <- mm[mmr,mmc, drop=FALSE]
      rrj <- setNames(drop(mmpart %*% lcf), rnn[mmr]) ## coef[mmc]
      if (se) {
        sej <- sqrt(diag(mmpart %*% cov[mmc,mmc] %*% t(mmpart)))
        if (any(is.na(rrj))|any(!is.finite(sej))) {
          ljfail <- c(ljfail, tl[j])
        } else {
          lscatter <- getscalepar(object)
          rrj <- inference(rrj, se=sej, df=df,
                           stcoef=rrj*0.5/lscatter, rlv=rlv, ...)
          li <- match(c("Rle","Rlp","Rls"), names(rrj))
          names(rrj)[li] <- c("coefRle","coefRlp","coefRls")
        }
      }
      res[[j]] <- rrj
    }
  }
  if (length(ljfail))
    warning(":termeffects: error calculating se for terms  ",
            paste(ljfail, collapse=", "))
  if (int > 0) {
    res <- c(list(`(Intercept)` = coef[int]), res)
  }
##-   if (inherits(object, "polr")) {
##-     lcfi <- object$intercepts[,1]
##-     res <- c(res,
##-              list("(Intercepts)"=inference(lcfi, object$intercepts[,2], df=df, stcoef=lcfi) ))
##-   }
  ##  class(res) <- "termeffects" ## don't do that:
  ##                                 want to be able to print the whole table
  structure(res, class="termeffects", head=lmethod, rlv.threshold=rlv.threshold)
} ## end termeffects
## -------------------------------------------------------------------------
"[.termeffects" <- #f
  function(x, i=NULL)  structure(unclass(x)[i], class="termeffects")
    ## unclass  avoids infinite recursion!
## ============================================================================
qintpol <- function(p, par1, par2, dist="binom")
{
  ## interpolated (theoretical) quantile for discrete distributions
  lqfn <- get(paste("q",dist,sep=""))
  lpfn <- get(paste("p",dist,sep=""))
  lq <- lqfn(p, par1, par2)
  lp <- lpfn(lq, par1, par2)
  lmaxx <- if (dist=="binom") par1 else Inf
  lp0 <- lpfn(pmin(pmax(0,lq-1),lmaxx), par1, par2)
  lq - (lp-p)/(lp-lp0)
}
## ------------------------------------------------------------------
confintF <- #f
  function(f, df1, df2=Inf, testlevel=0.05) {
  ## confidence interval for non-centrality of F distribution
  p <- testlevel/2
  lf.fq <- function(x, fvalue, df1, df2, p) qf(p,df1,df2,x)-fvalue
  lf.ciup <- function(fvalue, df, p) { ## upper bound for upper limit
    lq <- 1.5*qnorm(p)
    lu <- lq^2*2/df
    df*(fvalue-1+lu+sqrt(lu*(lu+2*fvalue-1)))
  }
  ln <- max(length(f), length(df1), length(df2), length(p))
  f <- rep(f, length=ln)
  df1 <- rep(df1, length=ln)
  df2 <- rep(df2, length=ln)
  p <- rep(p, length=ln)
  p <- pmin(p,1-p)
  ## ---------------------------
  rr <- matrix(NA, ln, 2)
  for (li in 1:ln) {
    lx <- f[li]
    if (!is.finite(lx)) next
    if (lx>100)
      rr[li,] <- df1[li]*(sqrt(lx)+c(-1,1)*abs(qt(p[li],df2[li]))/sqrt(df1[li]))^2
    else {
      ldf1i <- df1[li]
      if (ldf1i==0) next
      rr[li,1] <- { ## lower limit
        lf0 <- lf.fq(0, f[li], ldf1i, df2[li], 1-p[li])
        if (lf0>=0) 0
        else
          pmax(0, ## !!! something wrong, should not be needed
               uniroot(lf.fq, c(0,df1[li]*f[li]),
                       fvalue=f[li], df1=ldf1i, df2=df2[li], p=1-p[li])$root
               )
      }
      rr[li,2] <- ## upper limit
        if (pf(f[li], ldf1i, df2[li])<=p[li]) 0  ## tiny F value
        else
        uniroot(lf.fq, interval=c(df1[li]*f[li], lf.ciup(f[li], df1[li], 1-p[li])),
                fvalue=f[li], df1=ldf1i, df2=df2[li], p=p[li], extendInt="upX")$root
    }
  }
  if (ln==1) c(rr) else rr
}
## ===========================================================================
print.inference <- #f
  function (x, show = getOption("show.inference"), print=TRUE,
            digits = getOption("digits.reduced"), 
            transpose.ok = TRUE, legend = NULL, na.print = getOption("na.print"),
            ...)
{
  lf.format <- function(x, header="")
  { ## format matrix -> character vector including names
    x <- format(x)
    x <-
      if (length(dim(x))==0) 
        c(header,
          apply(format(rbind(names(x),x)), 1, paste, collapse="  ") )
      else {
        lcnm <- colnames(x)
        ltb <- rbind(lcnm,x)
        lrnm <- c(if(length(lcnm)) "", row.names(x))
        c(header, ## , rep("", ncol(x)-1+(length(lrnm)>0))),
          paste(apply(format(cbind(lrnm, ltb), justify="right"),
                      1, paste, collapse=" "), "\n", sep="")) 
##-         cbind(format(c(header, rep("", nrow(x)))),
##-               format(c("",rownames(x))),
##-               format(rbind(colnames(x), x)), "\n")
      }
    unname(x) ## apply(x, 1, paste, collapse="  "))
  }
  ## -------------------------------------
  ## show what?
  lsh <- c(show.signif.stars = getOption("show.signif.stars"),
           show.symbollegend = getOption("show.symbollegend"),
           show.rlv.threshold = getOption("show.rlv.threshold"),
           show.rlv.class = getOption("show.Rlv.class"))
  legend <- i.def(legend, lsh, structure(rep(TRUE, length(lsh)), names=names(lsh)))
  lItable <- length(dim(x))
  type <- i.def(attr(x,"type"), "simple")
  lshow <- if (type=="model") {
             lsh <- if (is.list(show)) show[1:2] else list(show, show)
             list(i.getshow(lsh[[1]], "terms", x=x$termtable),
                  i.getshow(lsh[[2]], "termeffects", x=x$termeffects))
           } else  i.getshow(show, type, x=x)
  ## --- head
  ldn <- attr(x, "data.name")
  if (!(is.character(ldn)&length(ldn)==1)) ldn <- NULL
  lout <- c(
    if (length(ldn)) paste("data: ", ldn),
    if (length(lfo <- attr(x, "formula")))
      paste("target variable: ", format(lfo[[2]]) ) ## was format()
  )
  lhead <- c(attr(x, "method"),
          if (length(ldist <- attr(x, "distribution")))
            c(" ;  distribution: ", ldist), "\n",
          if (length(lout)) paste(paste(lout, collapse=" ;  ")))
  ## lhead <- if (any(nchar(lhead)>2)) c(lhead,"\n")
  lshe <- getOption("show.estimate")
  lshet <- if(is.logical(lshe)) lshe else length(lshe)>0 
  if (lshet && length(lest <- attr(x, "estimate"))) {
    if (is.data.frame(lest)) lest <- list(lest)
    lhead <- c(lhead, "estimate:\n")
    lnm <- names(lest)
    for (ll in seq_along(lest)) {
      lle <- lest[[ll]]
      if (length(lcn <- colnames(lle))) 
      if (!is.logical(lshe)) lle <- lle[, lcn%in%lshe]
      if (length(lle))
        lhead <-
          c(lhead, lf.format(lle,
                             header= if (length(lnm))
                                    paste(lnm[ll],"\n",sep=":")))
    }
  }
  ## ---
  lleg <- NULL
  lx <- x
  ## ---------------------------------------------------------------
  if (type=="model") {
    lout <- print.coeftable(x$termtable, show=lshow[[1]], digits=digits,
                            transpose.ok=FALSE, na.print=na.print, print=FALSE)
    lte <- x$termeffects
    if (!u.true(single)) lte <- lte[sapply(lte, function(x) NROW(x)>1)]
    if (length(lte)) {
    ltep <- lapply(lte, print.coeftable, show=lshow[[2]], digits=digits,
                   transpose.ok=transpose.ok, na.print=na.print, print=FALSE)
    lout <-
      list(termtable=structure(lout, tail="\n"), termeffects=ltep)
    }
  } else { ## --------------------------------
  if (!lItable) {
    lout <- c(
      if (!is.na(leff <- x["effect"]))
        paste(if(length(leffn <- attr(x, "effectname")))
          paste(leffn, ": ",sep="") else "effect:   ", format(leff)),
      if (getOption("show.confint")) {
        lci <- x[c("ciLow","ciUp")]
        if (any(!is.na(lci)))
          paste(" ;  confidence int.: [",
                paste(format(lci), collapse=", "),"]\n")
        else "\n"
      } else "\n"
    )
    ## ---
    if (any(c("teststatistic","p.value","Sig0")%in%lshow)) {
      lpv <- x["p.value"]
      lps <-
        if (length(lpv)& ("p.symbol"%in%lshow | getOption("show.signif.stars")) )
          symnum(lpv, p.symbols$cutpoint, p.symbols$symbol)
      llout <- c(
        paste("Test:     hypothesis: effect = ", attr(x,"hypothesis"), "\n  "),
        paste(c(if(length(ltst <- attr(x, "teststatistic")))
                  paste("teststatistic: ", round(ltst, digits)),
                ## !!! df
                if("p.value"%in%lshow && length(lpv <- x["p.value"]))
                  paste("p value: ", round(lpv, digits+1), lps),
                if("Sig0"%in%lshow&&length(lsig <- x["Sig0"]))
                  paste("Sig0: ", round(lsig, digits),
                if (!"p.value"%in%lshow) lps)), collapse=" ;  ")
      )
      lout <-
        c(lout, if (length(llout)) paste(paste(llout, collapse=""), "\n") )
    }
  ## ---
    if (any(substring(lshow,1,2)=="Rl")) {
      lrs <- if ("Rls.symbol"%in%lshow)
               symnum(x["Rls"], rlv.symbols$cutpoint, rlv.symbols$symbol)
      llout <- c(
        if("Rle"%in%lshow) paste("Rle: ", round(x["Rle"], digits)),
        if("Rlp"%in%lshow) paste("Rlp: ", round(x["Rlp"], digits)),
        if("Rls"%in%lshow) paste("Rls: ", round(x["Rls"], digits), lrs),
        if("Rlv.class"%in%lshow) paste("Rlv.class: ", attr(x, "rlvclass"))
      )
      if (length(llout)) lout <- c(lout, paste(paste(llout, collapse=" ;  "), "\n") )
    }
  } else { ## ========================  table
    lout <- print.coeftable(x, lshow, print=FALSE)
  }
  }
  ## --- legend(s)
  if(u.notfalse(legend)) {
    lsh <- unlist(lshow)
    lrlv <- length(intersect(grep("Rl", lsh), grep(".symbol", lsh))) > 0
    lleg <- c(
      if(u.true(legend["show.signif.stars"]) && "p.symbol"%in%lsh)
        paste("Significance codes for p.value:  ", i.symleg(getOption("p.symbols")),"\n"),
      if(u.true(legend["show.symbollegend"]) && lrlv)
        paste("Relevance codes:    ", i.symleg(getOption("rlv.symbols")),"\n"),
      if(u.true(legend["show.rlv.threshold"]) && lrlv &&
         length(ll <- attr(lx, "rlv.threshold")))
        paste("Relevance threshold", if(length(ll)>1) "s",":  ",
              paste(paste(names(ll),as.character(ll), sep=" = "),collapse=", "),"\n", sep="")
      )
##-               ll, ";  type: ",
##-               attr(lx,"rlv.type"))
  }
  ltail <- c(lleg,
             if (type=="model")
               c("\n", print.modelextras(x$summary, digits=digits, print=FALSE) ))
  ## ---
  rr <- structure(lout, class=c("printInference", class(lout)),
                  head=lhead, tail=ltail)
  if (print) print.printInference(rr)
  invisible(rr)
}
## -----------------------------------------------------------------------------
print.coeftable <- #f
  function(x, show, print=TRUE,
            digits = getOption("digits.reduced"), 
            transpose.ok = TRUE, na.print = getOption("na.print") )
{
  lf.mergesy <- function(lx, x, var, place, concatenate=FALSE)
  {
    lxx <- x[,var]
    ltypep <- substring(var,1,3)=="p.v"
    lxx[is.na(lxx)] <- ltypep
    lsymb <- if(ltypep) p.symbols else rlv.symbols
    ls <- symnum(lxx, lsymb$cutpoint, lsymb$symbol)
    lsy <- format(c("    ",ls))[-1]
    ## trick needed to ensure flush left priniting of the symbols
    lcl <- match(c(var, place, "coef", "coef..sy "), colnames(lx), nomatch=0)
    lcl <- lcl[lcl>0][1]
    if (concatenate) {
      rr <- lx
      rr[,lcl] <- paste(formatNA(lx[,lcl], na.print=" .", digits=digits),lsy)
      names(rr)[lcl] <- paste(names(lx)[lcl], "..sy ", sep="")
    } else {
      rr <- if(lcl<ncol(lx))
              cbind(lx[,1:lcl,drop=FALSE], lsy, lx[,-(1:lcl),drop=FALSE])
            else  cbind(lx[,1:lcl,drop=FALSE], lsy)
      names(rr)[lcl+1] <-
        if (ltypep) "p.symbol" else paste(var, "symbol", sep=".")
      ## paste(substring(var,1,1),if (!ltypep) "R", "sy",sep="")
    }
    ## attr(rr, if(ltypep) "pLegend" else "rlvLegend") <- attr(ls, "legend")
    rr
  }
  ## -------------------------------------------------------------
  if (length(x)<=1) return(format(x)) ## usually this applies to "(Intercept)"
  x <- rbind(x)
  show[show=="estimate"] <- "coef"
  colnames(x)[colnames(x)=="estimate"] <- "coef"
  lshow <- intersect(show, c(colnames(x), relevance.symbnames)) 
##-   if (any(li <- !lshow%in%c(colnames(x),lsymbnm,"test"))) 
##-     warning(":print.inference: ", paste(lshow[li], collapse=", "),
##-             "  not available")
  lcols <- intersect(lshow, colnames(x))
  if (length(lcols)==0) {
    warning(":print.inference: no existing columns selected")
    return(invisible(x))
  }
  lx <- as.data.frame(x)[,lcols, drop=FALSE]
  ## --- round some columns to 3 digits
  ljrp <- lcols[pmatch(c("R2","p.v"), lcols, nomatch=0)]
  if (length(ljrp)) lx[,ljrp] <- round(lx[,ljrp],digits) ## as.matrix()
  ljrp <- lcols[c(grep("Rl", lcols),grep("Sig", lcols))]
  if (length(ljrp)) lx[,ljrp] <- round(lx[,ljrp],i.last(digits)-1)
  ## --- paste symbols to numbers
  lIsymb <- FALSE
  if ("p.symbol"%in%lshow & "p.value"%in%colnames(x)) {
    lx <- lf.mergesy(lx, x, "p.value", "Sig0", concatenate=TRUE)
    lshow <- setdiff(lshow, "p.symbol")
    lIsymb <- TRUE
  }
  li <- grep("Rls.symbol", lshow)
  if (length(li)) {
    lrs <- lshow[li]
    lrc <- sub(".symbol","",lrs)
    for (lii in seq_along(li)) {
      lvar <- lrc[lii]
      if (!lvar%in%names(x)) {
        warning(":print.inference: column ",lvar, " not available")
        next
      }
      lx <- lf.mergesy(lx, x, lvar, c("dropRls", "coefRls", "coef"),
                       concatenate=TRUE) ## lvar%in%lshow
      ## should be FALSE for something like Rlp.symbol if Rlp is not in show
      lshow <- setdiff(lshow, lvar)
    }
    lIsymb <- TRUE
  }
  ## result
  rr <-
    if (transpose.ok && ncol(lx)==1) 
      setNames(lx[[1]], paste(row.names(lx), if (lIsymb) "    ")) 
    else setNames(formatNA(lx, na.print=na.print, digits=digits), names(lx))
     ## apply(format(lx), 2, function(x) sub("NA", na.print, x))
  if (print) print.printInference(rr, quote=FALSE)
  invisible(rr) ## structure(rr, show=lshow)
}
## --------------------------------------      
print.printInference <- #f
  function(x, ...)
{
  ltail <- NULL
  if (is.list(x)&!is.data.frame(x)) { ## print global head
    if (length(lt <- attr(x,"head")))
      cat(lt, "\n", sep="")
    ##---
    ltail <- attr(x,"tail")
  } else x <- list(x)
  lInam <- length(lnam <- names(x))
  ## -------------------------------------
  for (li in seq_along(x)) {
    if (lInam) cat("$", lnam[li], "\n")
    lx <- x[[li]]
    class(lx) <- setdiff(class(lx), c("printInference", "inference"))
    ## attr(lx, "tail") <- NULL
    ## print
    if (length(lhead <- attr(lx,"head"))) cat(lhead, "\n", sep="")
    ## attr(lx, "head") <- NULL
    if (length(dim(lx))) print(as.data.frame(lx), ...)
    ## as.data.frame justifies the (character) columns correctly
    else {
      if(length(names(lx))) print(c(lx), quote=FALSE, ...)
      else cat(if(is.character(lx)) lx else format(lx), "\n", sep="")
    }
    if (length(ltl <- attr(lx,"tail"))) cat(ltl) ## cat("\n", ltl, sep="")
  ##   cat("\n")
  }
  if (length(ltail)) cat(paste(ltail, collapse="")) ## cat("\n", ltail, sep="") 
  ##  cat("\n")
}
## -----------------------------------------------------------
i.getshow <- #f
  function(show, type=c("simple", "terms", "termeffects"), x=NULL)
{ ## collect items to be shown by print.inference
  if (identical(show, "all")) {
    if(0==length(lcn <- colnames(x))) {
      warning(":getshow: no columns found. Using default")
      show <- NULL
    } else return(lcn)
  }
  lcoll <- intersect(show, c("test", "relevance", "classical"))
  if(length(lcoll)) {
    ltype <- pmatch(type, c("simple", "terms", "termeffects"), nomatch=0)
    ## if (length(ltype)==0)
    lc <- paste("show", c("simple","terms","termeffects")[ltype], lcoll, sep=".")
    for (l in lc)
      show <- c(show, getOption(l))
  }
  if (all(c("nocoef","coef")%nin%show)) show <- c("coef", show)
  ## if (any(c("teststatistic","p.value","Sig0")%in%show)) show <- c(show,"test")
  setdiff(show,lcoll)
}
## =============================================================================
getcoeftable <-
  function (object)
{ ## get coefficient table from model fit
  if (inherits(object, "regr")) ltb <- object$coeftable
  else {
    if (inherits(object, "survreg")) {
      ltb <- summary(object)$table
  ##    ltb <- ltb[-nrow(ltb),]
    } else {
      if (inherits(object, "coxph")) {
        lcoef <- object$coefficients
        se <- sqrt(diag(object$var))
        ltb <- cbind(lcoef, se, lcoef/se,  ## exp(lcoef),
                     pchisq((lcoef/se)^2, 1, lower.tail = FALSE))
        dimnames(ltb) <-
          list(names(lcoef), c("coef", "se", "z", "p")) ## "exp(coef)",
      } else  ltb <- summary(object)$coefficients
    }
    ldf <-
      if (class(object)[1]%in%c("rlm", "coxph"))
        length(object$residuals)-length(object$coef)
      else df.residual(object)
    if (inherits(object, "polr")) ltb <- ltb[names(object$coefficients),]
  }
  lnm <- names(coefficients(object))
  if (any(is.na(match(lnm,row.names(ltb))))) {
    rr <- structure(matrix(NA, length(lnm), ncol(ltb)), dimnames=list(lnm,colnames(ltb)))
    rr[row.names(ltb),] <- ltb
  } else rr <- ltb
  structure(rr, df=ldf)
}
## --------------------------------------------------------------------------------
print.termtable <- #f
  function (x, show = getOption("show.inference"), ...)
{
  show <- i.getshow(show, "terms", x=x)
  print.inference(x, show=show, ...)
}
## --------------------------------------------------------------------------------
print.termeffects <- #f
  function (x, show = getOption("show.inference"), transpose.ok=TRUE,
            single=FALSE, print = TRUE, warn = TRUE, ...)
{
  lshowall <- length(show)==1 && show=="all"
  show <- i.getshow(show, "termeffects", x=x)
  lx <- if (single) x else x[sapply(x, function(x) NROW(x)>1)]
  if (length(lx)==0) {
    if (warn) warning(":print.termeffects: no termeffects",
                      if (!single) " with >1 degree of freedom")
    return()
  }
  for (li in seq_along(lx)) {
    lxx <- lx[[li]]
    lsh <- if(lshowall) c(colnames(lxx), relevance.symbnames) else show
    lr <-
      if (length(lxx)>1) {
        print.inference(lxx, show=lsh, transpose.ok=transpose.ok, print=FALSE)
      } else format(lxx) ## e.g., "(Intercept)"
    ltail <- attr(lr,"tail")
    attr(lr, "head") <- attr(lr,"tail") <- NULL
    if (length(lr)==1) lr <- paste("   ", lr)
    lx[[li]] <- lr
  }
  rr <- structure(lx, class="printInference",
                  head=paste(attr(x, "head"),"\n"), tail=ltail)
  if (print) print.printInference(rr)
  invisible(rr)
}
## -------------------------------------------------------------
print.modelextras <- #f
  function(x, digits=getOption("digits"), print=TRUE)
{
  lf.form <- function(x, header="", digits=digits)
    paste(header,":  ", formatC(x, digits = digits), sep="")
  rr <- NULL
  ## error
  rdf <- x$df[2] ## df.residual(x)
  if (length(lsig <- x$scatter) && !u.true(attr(lsig,"fixed")))
    rr <-
      c(rr, scatter = lf.form(lsig, "St.dev.error", digits = digits),
        df.residual = paste("  on ", rdf, " degrees of freedom") )
  if (length(ldp <- x$dispersion))
    rr <-
      c(rr, dispersion = 
              lf.form(ldp, paste("dispersion parameter: ",
                                 if (u.true(attr(ldp,"fixed"))) "fixed at "),
                      digits=digits) )
  if (length(lsc <- x$scatter))
    rr <-
      c(rr, scatter =
              lf.form(lsc, paste("shape parameter ('scatter')",
                                 if (u.true(attr(ldp,"fixed"))) "fixed at "),
                      digits=digits) )
  ##  if (length(lout)) rr <- c(rr, lout, "\n")
  rr <- c(rr, "\n")
  lr2 <- x$r.squared
  if (length(lr2)&&!is.na(lr2))
    rr <-
      c(rr,
        R2 = paste(
          c(
            lf.form(lr2, "Multiple R^2", digits=digits),
            if (length(lr2a <- x$adj.r.squared))
              lf.form(lr2a, "Adjusted R^2", digits = digits) ),
          collapse=";  "),"\n")
  if (length(lAIC <- x$AIC)&&!is.na(lAIC))
    rr <- c(rr, AIC =
                  lf.form(lAIC, "AIC", digits = log10(abs(lAIC))+3) )
  if (length(lfst <- x$fstatistic)>0) 
    rr <-
      c(rr, fstatistic =
              paste(lf.form(lfst[1], "F-statistic",digits = digits),
                    "  on", x$fstatistic[2], "and", x$fstatistic[3],
                    "d.f.; ",
                    lf.form(pf(x$fstatistic[1], x$fstatistic[2], x$fstatistic[3],
                               lower.tail=FALSE), "p.value", digits = digits),
                    "\n") )
  ##   cat("\nDistribution: ", x$distrname)
  if (print) print(rr, quote=FALSE)
  invisible(rr)
}
## =========================================================================
i.prep.plinference <- #f
  function(x, overlap) {
  if (is.null(dim(x))) x <- rbind(x)
  lnm <- colnames(x)
  lj <- match(c("Rle","Rls","Rlp"), lnm)
  if (any(is.na(lj)))
    lj <- match(c("coefRle","coefRls","coefRlp"), lnm)
  if (any(is.na(lj)))
    stop("!plot.inference! Rle, Rls, Rlp not found")
  x <- x[,lj, drop=FALSE]
  if (all(is.na(x)))
    stop("!plot.inference! no finite values for Rle, Rls, Rlp")
  if (nrow(x)>1 & overlap) {
    loverlapfactor <-
      if (nrow(x)>2)
        0.707 else { ## only two intervals to compare
                lqse <- abs(x[,3]-x[,1])
                sqrt(sum(lqse^2))/sum(lqse)
              }
    x <- cbind(x, x[,1]+loverlapfactor*(x[,2:3]-x[,1]) )
  }
  x
}
## -----------------------------------------------------------------
plot.inference <- #f
  function(x, pos = NULL, overlap = FALSE, refline = c(0, 1, -1),
           xlab="relevance", ...) ## sub=NULL, 
{
  x <- i.prep.plinference(x, overlap)
  plconfint(x, pos=pos, xlab = "relevance", ...)
  if (length(refline))
    abline(v=refline, lwd=i.def(attr(refline, "lwd"),2),
           col=i.def(attr(refline, "col"), "gray70"))
  invisible(x)
}
## ----------------------------------------------------------------
plconfint <- #f
  function(x, y = NULL, select=NULL, overlap = NULL, pos = NULL,
           xlim = NULL, refline = 0, add = FALSE, bty = "L", col = NULL,
           plpars=list(lwd=c(2,3,1,4,2), posdiff=0.35,
                       markheight=c(1,0.6,0.6), 
                       extend=NA, reflinecol = "gray70"),
           xlab="", ...)
{
  lcheck <-
    list(x=cnr(na.ok=FALSE, dim=c(1,3)), y=cnr(),
         select=cnr(range=c(0,Inf)), overlap=clg(), pos=cnr(),
         xlim=cnr(), add=clg(),
         bty=cch(), col=ccl(), plpars=cls(), xlab=cch()
         )
  largs <- check.args(lcheck, envir=parent.frame())
  for (lnm in names(largs)) assign(lnm, largs[[lnm]])
  ## --------------
  i.extendrange <-
    function(range, ext=0.04)  range + c(-1,1)*ext*diff(range)
  ## ---
  if (inherits(x, "inference")) x <- x[,c("Rle","Rls","Rlp")] ## x[,c("effect", "se")]
  lx <- as.matrix(rbind(x))
  if (ncol(lx)==2) lx <- lx[,1] + outer(lx[,2], c(0,-1,1))
  if (length(select)) lx <- lx[select,]
  lnx <- nrow(lx)
  ## ---
  lnmeff <- i.def(row.names(lx), "")
  lposlb <- lpos <- if(length(pos)) pos else lnx:1
  lcol <- rep(i.def(col,1), length=lnx)
  if (length(y)) {
    if (inherits(y, "inference")) y <- y[,c("Rle","Rls","Rlp")]
    ly <- as.matrix(rbind(y))
    if (length(select)) ly <- ly[select,]
    lny <- nrow(ly)
    if (lny!=lnx)
      stop("!plconfint! arguments 'x' and 'y' do not match")
    if (ncol(ly)==2) ly <- ly[,1] + outer(ly[,2], c(0,-1,1))
    if (u.notfalse(overlap)) {
      lxw <- (lx[,3]-lx[,2])/2
      lyw <- (ly[,3]-ly[,2])/2
      lovfac <- sqrt(lxw^2+lyw^2)/(lxw+lyw)
      lx <- cbind(lx[,1:3,drop=FALSE],
                  lx[,1] + lovfac*(lx[,2:3,drop=FALSE]-lx[,1]))
      ly <- cbind(ly[,1:3,drop=FALSE],
                  ly[,1] + lovfac*(ly[,2:3,drop=FALSE]-ly[,1]))
    }
    if (ncol(lx)!=ncol(ly))
      stop("!plconfint! number of columns of 'x' and 'y' are different") 
    lx <- rbind(lx,ly)[c(outer(c(0,lnx), 1:lnx, "+")),]
    if (length(lpos)==lnx) {
      lmd <- if (lnx>2) min(diff(sort(lpos))) else 1
      lpos <- c(outer(lmd*plpars$posdiff/2*c(1,-1), lpos, "+"))
    }
    lnx <- 2*lnx
    lcol <- rep(i.def(col,c("blue","red")), length=lnx)
  }
  ## ---
  if (nrow(lx)==0)
    stop("!plconfint! no elements left for argument ''x'")
  ## ---
  l2ci <- ncol(lx)>=5
  if (!u.true(single)) {
    li <- !is.na(lx[,1])
    lx <- lx[li,, drop=FALSE]
  lpos <- lpos[li]
  lcol <- lcol[li]
  }
  if (length(lx)==0L)
    stop("!plconfint! no intervals to plot")
  lwd <- rep(i.def(plpars[["lwd"]],2), length=5)
  lmh <- 0.1 * rep(c(plpars[["markheight"]],1),length=3) ## * diff(range(lpos))/ln 
  if (!add) {
##    rr <- replication(x,y)
## range
    lrg <- i.extendrange(range(c(lx), na.rm=TRUE))
    if (length(xlim)) {
      if (length(xlim)!=2) 
        warning(":plconfint: argument 'xlim' not suitable")
      else lrg[!is.na(xlim)] <- xlim[!is.na(xlim)]
    }
    lxt <- i.def(plpars[["extend"]], 1/lnx)
    lposlim <- matrix(c(1+lxt, -lxt, -lxt, 1+lxt),2)%*%range(lpos)
    lmar <- par("mar")
    lnch <- max(nchar(lnmeff))
    lmar[2] <- 0.7*lnch+1
    loldp <- par(mar=lmar)
    on.exit(par(loldp))
    plot(lrg, lposlim, yaxs="i", type="n", axes=FALSE,
         xlab=xlab, ylab="", xaxs="i", yaxs="i", ...) # c(min(0,lrg[1]), max(1,lrg[2]))
    lrlcol <- plpars[["reflinecol"]]
    box(bty=bty, col=lrlcol)
    axis(1, col=lrlcol)
    if (length(refline)) abline(v=refline, col=plpars$reflinecol)
  }
  segments(lx[,2],lpos, lx[,3],lpos, lwd=lwd[1], col=lcol) ## interval line
  segments(lx[,1],lpos-lmh[1],lx[,1],lpos+lmh[1], lwd=lwd[2], col=lcol) ## midpoint = estimate
  segments(lx[,2], lpos-lmh[2], lx[,2], lpos+lmh[2],
           lwd=lwd[3], col=lcol) ## endmarks
  segments(lx[,3], lpos-lmh[2], lx[,3], lpos+lmh[2],
           lwd=lwd[3], col=lcol) ## endmarks
  if (l2ci) {
    segments(lx[,4],lpos, lx[,5],lpos, lwd=lwd[4], col=lcol) ## interval line
    segments(lx[,4], lpos-lmh[2], lx[,4], lpos+lmh[2],
             lwd=lwd[5], col=lcol) ## endmarks
    segments(lx[,5], lpos-lmh[2], lx[,5], lpos+lmh[2],
             lwd=lwd[5], col=lcol) ## endmarks
  }
  ## ---
  mtext(lnmeff, side=2, at=lposlb, line=1, adj=1, las=1)
}
## ---------------------------------------------------------------
pltwosamples <- function(x, ...) UseMethod("pltwosamples")
## ---
pltwosamples.default <- #f
  function(x, y = NULL, overlap = TRUE, ...) ## , sub=":"
{
  lcheck <-
    list(x=list(cnr(na.ok=FALSE),cdf()), y=cnr(), overlap=clg()
         )
##-   lcall <- match.call(expand.dots = FALSE)
##-   lcall$... <- NULL
  largs <- check.args(lcheck, envir=parent.frame())
  for (lnm in names(largs)) assign(lnm, largs[[lnm]])
  ## --------------
  if (is.matrix(x)) x <- as.data.frame(x)
  if (inherits(x, "list")) {
    ##     stop("!pltwosamples! First argument not suitable")
    lnm <- names(x)
    y <- x[[2]]
    x <- x[[1]]
  }
  if (length(y)==0)
    stop("!pltwosamples! Argument 'y' not specified")
  lx <- onesample(x)
  ly <- onesample(y)
  plconfint(c(lx[c("effect","ciLow","ciUp","se")],n=attr(lx,"n")[1]),
            c(ly[c("effect","ciLow","ciUp","se")],n=attr(ly,"n")[1]),
                  overlap=overlap, ...)
}
## -------------------------------------------------------------
pltwosamples.formula <- #f
  function(formula, data=NULL, ...) ## pos = NULL, col=1, 
{
  if (length(formula) != 3L)
    stop("!twosamples! 'formula' must have left and right term")
  oneSampleOrPaired <- FALSE
  if (length(attr(terms(formula[-2L]), "term.labels")) != 1L)
    if (formula[[3]] == 1L)
      oneSampleOrPaired <- TRUE
    else stop("!twosamples! 'formula' incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  mf <- eval(m, parent.frame())
  ll <- split(mf[,1],mf[,2])
  pltwosamples.default(ll, ...)
}
## ---------------------------------------------------------------
plot.termeffects <- #f
  function(x, pos = NULL, single=FALSE, overlap = TRUE,
           termeffects.gap = 0.2, refline = c(0, 1, -1),
           xlim=NULL, ylim=NULL, xlab = "relevance", mar=NA,
           labellength = getOption("labellength"), ...)
{
  lcheck <-
    list(x=cnr(na.ok=FALSE), single=clg(), overlap=clg(), 
         termeffects.gap=cnr(range=c(-1,10)),
         labellength=cnr(range=c(0,50))
         )
##-   lcall <- match.call(expand.dots = FALSE)
##-   lcall$... <- NULL
  largs <- check.args(lcheck, envir=parent.frame())
  for (lnm in names(largs)) assign(lnm, largs[[lnm]])
  ## --------------
  mardefault <- par("mar")
  li <- sapply(x, is.atomic)
  x <- x[!li]  ## (Intercept)
  lx <- lapply(x, function(x) x <- x[is.finite(x[,"effect"]),,drop=FALSE])
  llen <- sapply(lx, nrow)
  lx <- lx[llen>0]
  llen <- llen[llen>0]
  if (!single) {
    lx <- lx[llen>1]
    llen <- llen[llen>1]
  }
  if (length(llen)==0)
    stop("!plot.termeffects! No termeffects", if(!single) "with length >1")
  lnx <- length(lx)
  lnmx <- names(lx)
  lxx <- matrix(,0,3)
  llb <- NULL
  for (ll in seq_along(lx)) {
    llx <- rbind(lx[[ll]])
    if (llen[ll] <- nrow(llx)) {
      lxs <- llx[,c("coefRle","coefRls","coefRlp")]*sign(llx[,"estimate"])
      lxx <- rbind(lxx, lxs)
      llb <- c(llb, if(llen[ll]>1) row.names(llx) else lnmx[ll])
    }
  }
  if (is.null(pos)) {
    pos <-
      rep(termeffects.gap*(1:length(llen)-(lnx==1))+
          cumsum(llen>1), llen) + 1:sum(llen)
    pos <- max(pos)+1-pos
  }
  if (length(xlim)==0) xlim <- range(unlist(lxx), na.rm=TRUE)
  if (length(ylim)==0) ylim <- range(pos) + c(0, llen[1]>1 & lnx>1)
  if (length(mar)==1) mar <- c(NA, mar, NA,NA)
  if (length(mar)&&is.na(mar[2])) {
    lmr2 <- max(nchar(llb))
    if (lmr2>labellength) {
      llb <- sub(")","", sub("log.*\\((.*)\\)","l:\\1", llb))
      llb <- shortenstring(llb, labellength)
      lmr2 <- labellength
    }
    mar[2] <- lmr2*0.7 + 1
  }
  mar <-
    if (length(mar))
      ifelse(is.finite(mar), mar, mardefault) else mardefault
  ## ---
  lop <- par(mar=mar)
  plot(xlim, ylim, xlab=xlab, ylab="", axes=FALSE, type="n")
  on.exit(par(lop))
  axis(1)
  box()
  lx0 <- xlim[1]
  li0 <- 0
  lnmx <- names(x)
  for (lt in 1:lnx) {
    ltn <- llen[lt]
    li <- li0+1:ltn
    llx <- lxx[li,]
    row.names(llx) <- row.names(lx[[lt]])
    if (ltn>1&lnx>1)
      text(lx0, pos[li0+1]+1, lnmx[lt], adj=0)
      else row.names(llx) <- llb[li]
    plconfint(llx, pos=pos[li], add=TRUE)
    li0 <- li0+ltn
  }
  if (length(refline))
    abline(v=refline, lwd=i.def(attr(refline, "lwd"),2),
             col=i.def(attr(refline, "col"), "gray70"))
}
## =============================================================================
i.logscale <- #f
  function(object)
{
  substring(as.character(formula(object)[[2]]),1,3)=="log" ||
    (inherits(object, "glm") &&
     object$family%in%
     c("binomial","poisson","quasibinomial","quasipoisson")) ||
    inherits(object,"survreg")&&object$dist!="gaussian"
}
## --------------------------------------------------------------------
getscalepar <- #f
  function(object)
{ ## get scale parameter of a fit
  lsry <- summary(object)
  rr <- c(lsry$rr, lsry$scatter)[1]
  if (length(rr)==0) rr <- sqrt(c(lsry$dispersion,1)[1])
  rr
}
## -----------------------------------------------------------
getcoeffactor <- #f
  function(object, standardize=TRUE)
{
  ## get factor for converting coef to coef effect
  ## model matrix
  lmmt <- object[["x"]]
  if (length(lmmt)==0)  object$x <- lmmt <- model.matrix(object)
  lfamily <- object$family$family
  ldist   <- object$dist
  lscatter <- if (standardize) getscalepar(object) else 1
  lfac <- apply(lmmt, 2, sd)/lscatter
  lfac[lfac==0] <- NA
  structure(lfac, scatter=lscatter, fitclass=class(object),
            family=lfamily, dist=ldist)
}
## -----------------------------------------------------------
rlvClass <- #f
  function(effect, ci=NULL, relevance=NA)
{ ## relevance class !!! ciwidth - interval !!!
  lrlvth <- i.def(relevance, getOption("rlv.threshold")[1])
  if (inherits(effect, "inference")) {
    if (length(ci))
      warning(":rlvClass: argument 'ci' ignored")
    rr <- attr(effect, "Rlv.class")
    if (length(rr))  return(rr) 
    rle <- effect["Rle"]
    rls <- effect["Rls"]
    rlp <- effect["Rlp"]
  } else {
    if (length(ci)==length(effect))
      ci <- effect + outer(c(ci), c(-1,1))
    if (length(ci)!=2*length(effect))
      stop("!rlvClass! lengths of 'estimate' and 'ci' not compatible")
    rle <- effect/lrlvth
    lcis <- rbind(ci/lrlvth)
    rls <- lcis[,1]
    rlp <- lcis[,2]
    names(rls) <- i.def(row.names(effect),
                            i.def(row.names(lcis), NULL) )
  }
  rr <- ifelse(rls>=1, "Rlv", "Amb.Sig")
  rr <- ifelse(rls<0, "Amb", rr) ## rls may be a vector
  rr <- ifelse(rlp<1, "Ngl", rr)
  rr <- ifelse(rlp<0, "Ctr", rr)
  rr
}
## ====================================================================
relevance.modelclasses <-
  c("regr","lm","lmrob","glm","polr","survreg","coxph")
## ,"rlm" : no correlation matrix of coef
## ,"rq"
p.symbols <- list(symbol=c("***", "**", "*", ".", " "),
                  cutpoint=c(0, 0.001, 0.01, 0.05, 0.1, 1) )
rlv.symbols <- list(symbol=c(" ", ".", "+", "++", "+++"),
                    cutpoint=c(-Inf,0,1,2,5,Inf) )
i.symleg <-
  function(x) paste(c(rbind(as.character(x$cutpoint),c(x$symbol,""))),
                    collapse="  ")
relevance.symbnames <-
  c("p.symbol", "coefRls.symbol", "dropRls.symbol", "predRls.symbol")

## -----------------------------------------------------------
relevance.options <- list(
  digits.reduced = 3,
  testlevel = 0.05,
  rlv.threshold =
    c(stand=0.1, rel=0.1, prop=0.1, corr=0.1, coef=0.1, drop=0.1, pred=0.05),
  termtable = TRUE, 
  show.confint = TRUE,
  show.estimate = c("estimate","scatter", "n", "effect","Rle","Rls","Rlp"),
  show.doc = TRUE, 
  show.inference = "relevance",
  show.simple.relevance = c("Rle", "Rlp", "Rls", "Rls.symbol", "Rlv.class"),
  show.simple.test = c("Sig0", "p.value", "p.symbol"),
  show.simple.classical = c("teststatistic", "p.value", "p.symbol"),  ## !!! symbols?
  show.terms.relevance = c("df", "R2x", "coefRlp", "coefRls", ## "dropRle",
                         "dropRls", "dropRls.symbol", "predRle"),
  show.terms.test = c("df", "ciLow","ciUp", "R2x", "Sig0", "p.value",
                         "p.symbol"),
  show.terms.classical = c("df", "se", "teststatistic", "p.value", "p.symbol"),
  show.termeffects.relevance = c("coef","coefRls.symbol"),
  show.termeffects.test = c("coef","p.symbol"),
  show.termeffects.classical = c("coef","p.symbol"),
  show.symbollegend = TRUE, show.rlv.threshold = TRUE,
  show.Rlv.class = TRUE,
  labellength = 8, 
  na.print = ". ",
  p.symbols = p.symbols,
  rlv.symbols = rlv.symbols
)
.onLoad <- function(lib, pkg) options(relevance.options)
## =============================================================================
