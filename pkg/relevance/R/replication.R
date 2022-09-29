replication <- #f
  function(x, y=NULL,
           testlevel=getOption("testlevel"),
           rlv.threshold=getOption("rlv.threshold"))
{
  lf.getv <- function(nm)
    if (nm%in%names(lx)) lx[,nm] else NA
  ltlev2 <- testlevel/2
  lrlvth <-
    i.def(rlv.threshold[1],
          c(getOption("rlv.threshold")[1],
            relevance.options[["rlv.threshold"]][1]))[1]
  latr <- lato <- list()
  if (length(y)) {
    latr <- attributes(y)
    lato <- attributes(x)
    y <- rbind(y)
    x <- rbind(x)
    if (nrow(y)!=nrow(x))
      stop("!replication! arguments 'x' and 'y' must have ",
           "the same dimension")
    x <-
        structure(cbind(x,y),
                  names=c(paste(names(x),"o",sep=""),
                          paste(names(y),"r",sep="") )
                  )
  }
  ## ------------
  lx <- as.data.frame(rbind(x))
    ##-     if (is.data.frame(x)) {
    ##-       ldnm <-
    ##-         c(outer(c("effect","n","teststatistic","se","scatter","df"),
    ##-                 c("o","r"), paste, sep=""))
    ##-       lj <- intersect(names(x), ldnm)
    ##-       if (length(lj)<=2)
    ##-         stop("!replication! Invalid first agrument")
    ##-       as.matrix(x[1,lj])
    ##-     } else
    ##- as.data.frame(rbind(x))
  leffo <- lf.getv("effecto")
  lno <- lf.getv("no")
  ltsto <- lf.getv("teststatistico")
  lseo <- i.def( i.def(lf.getv("seo"),leffo/ltsto), 1)
  ldfo <- i.def(lato$dfo, lno-1)
  lsco <- i.def(lf.getv("scattero"), lseo*sqrt(lno))
  ## ---
  leffr <- lf.getv("effectr")
  lnr <- lf.getv("nr")
  ltstr <- lf.getv("teststatisticr")
  lser <- i.def( i.def(lf.getv("ser"),leffr/ltstr), 1)
  ldfr <- i.def(latr$df, lnr-1)
  lscr <- i.def(lf.getv("scatterr"), lser*sqrt(lnr))
  ## --------------------
  leff <- (leffr-leffo)/2
  lse <- sqrt(lser^2+lseo^2)/2
  lsc <- (lsco+lscr)/2
  ltst <- leff/lse
  lpv <- pnorm(ltst)
  lq <- -qnorm(testlevel/2) ## qt(testlevel/2, min(lno,lnr)-1)
  lsig <- ltst/lq
  lciw <- lq*lse
  leffci <- leff+outer(lciw,c(-1,1))
  lrlvclass <- rlvClass(-leff, lciw, rlv.threshold[2])
  ## -----
  lsratio <- lsco/lscr ## if (length(sco)) sco/scr else 1
  lqt <- qt(0.975, lnr-1)
  lciwd <- lqt*sqrt(1/lnr+lsratio/lno)/2
  lrlvci <- cbind(leff,leffci)/lscr/lrlvth ## use scale of the replication
  lciwdpropr <- sqrt(1/lnr)/(sqrt(1/lnr)+sqrt(lsratio/lno))
  lciwo <- lqt*lsratio/(2*sqrt(lno))
  lciwr <- lqt/(2*sqrt(lnr))
  lclo <- rlvClass(leffo, lciwo)
  lclr <- rlvClass(leffr, lciwr)
  lclor <- cbind(original=lclo, replication=lclr)
  if (nrow(lclor)==1) lclor <- lclor[1,]
  lest <- cbind(
    effecto=leffo, effectr=leffr, seo=lseo, ser=lser, no=lno, nr=lnr,
    scattero=lsco, scatterr=lscr)
  lcl <- rplClass(lrlvclass, lclr, leffr)
  ## ---
  rr <- cbind(effect=leff, se=lse, teststatstic=ltst, p.value=lpv, Sig0=lsig,
      ciLow=leffci[1], ciUp=leffci[2],
      Rle=lrlvci[,1], Rls=lrlvci[,2], Rlp=lrlvci[,3])
  if (nrow(rr)==1) rr <- rr[1,]
  structure( rr,
    rplclass = lcl,
    class = c("inference", "replication"), type="simple",
    method = "replication", effectname = "effect difference",
    n = c(lno, lnr),
    estimate = lest,
    df = c(ldfo, ldfr),
    rlvclass = lclor, 
    rlv.threshold = lrlvth,
    rlv.type = "standardized"
  )
}  
## ------------------------------------------------------------------------
rplConfint <- ## f
  function(rr=list(),
           effecto=rr$effecto, teststatistico=rr$teststatistico,
           no=rr$no, scattero=rr$scattero, seo=rr$seo, 
           effectr=rr$effectr, teststatisticr=rr$teststatisticr,
           nr=rr$nr, scatterr=rr$scatterr, ser=rr$ser,
           rlv.threshold=rep(getOption("rlv.threshold")[1],2))
{ ## Confidence intervals for replication study and original
  if (length(no)*length(nr)==0)
    stop("'no' or 'nr' missing")
  if (length(effecto)==0) effecto <-teststatistico/(2*sqrt(no))
  if (length(scattero)==0) scattero <-seo*sqrt(no)/2
  if (length(effectr)==0) effectr <-teststatisticr/(2*sqrt(nr))
  if (length(scatterr)==0) scatterr <-ser*sqrt(nr)/2
  lsratio <- if (length(scattero)) scattero/scatterr else 1
  lqt <- qt(0.975, nr-1)
  lciwdpropr <- sqrt(1/nr)/(sqrt(1/nr)+sqrt(lsratio/no))
  lciwd <- lqt*sqrt(1/nr+lsratio/no)/2
  lciwo <- lqt*lsratio/(2*sqrt(no))
  lciwr <- lqt/(2*sqrt(nr))
  lrlvclasso <- rlvClass(effecto, lciwo, rlv.threshold[1])
  lrlvclassr <- rlvClass(effectr, lciwr, rlv.threshold[1])
  lrlvclassd <- rlvClass(effecto-effectr, lciwd, rlv.threshold[2])
  rr <- data.frame(
    no = no, nr = nr, effecto = effecto, effectr = effectr, effectd = effectr-effecto, 
    ciwo = lciwo, ciwr = lciwr, ciwd = lciwd,  
    cidwo = lciwd*(1-lciwdpropr),
    cidwr = lciwd*lciwdpropr,
    rlvclasso = lrlvclasso, rlvclassr = lrlvclassr, rlvclassd = lrlvclassd,
    rplclass = rplClass(lrlvclassr, lrlvclassd, effectr),
    row.names = row.names(rr)
  )
  rr
}  
## ------------------------------------------------------------
rplClass <- ## f
  function(rlvclassd, rlvclassr, rler=NULL)
{
  if (is.factor(rlvclassd)) rlvclassd <- as.character(rlvclassd)
  if (!is.character(rlvclassd)) {
    if (!inherits(rlvclassd, "inference"))
      rlvclassd <- inference(rlvclassd)
    rlvclassd <- attr(rlvclassd, "rlvclass")
  }
  if (is.factor(rlvclassr)) rlvclassr <- as.character(rlvclassr)
  if (!is.character(rlvclassr)) {
    if (!inherits(rlvclassr, "inference")) 
      rlvclassr <- inference(rlvclassr)
    rler <- i.def(rlvclassr["Rle"], Inf) ## !!! make sure that rler is not given or identical
    rlvclassr <- attr(rlvclassr, "rlvclass")
  }
  ## ------
  rr <-
    c("Cnf","CfnW","Att","Enh","Amb","Anh","Ctr")[
      match(rlvclassr, c("Rlv","Amb.Sig","Rlv","Rlv","Amb","Ngl","Ctr"))]
  rr <- ifelse(rr=="Cnf",
        ifelse(rlvclassd=="Rlv", "Att",
        ifelse(rlvclassd=="Ctr", "Enh", rr)), rr)
  rr <- ifelse(rr=="CfnW",
        ifelse(rlvclassd=="Rlv", "Att", 
        ifelse(rler<1, "Amb", rr)), rr)
  rr
}    
## -------------------------------------------------------------           
## fisherz <- function(r) 0.5*log((1+r)/(1-r))
## ===============================================================================
