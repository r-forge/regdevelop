replication <- #f
  function(original, replication,
           testlevel=getOption("testlevel"),
           rlv.threshold=getOption("rlv.threshold"))
{
  lcheck <-
    list(original=list(cnr(na.ok=FALSE), cdf()),
         y=list(cnr(na.ok=FALSE), cdf()),
         testlevel=cnr(range=c(0,0.5)),
         rlv.threshold=cnr(range=c(0,Inf)) )
  largs <- check.args(lcheck, envir=parent.frame())
  for (lnm in names(largs)) assign(lnm, largs[[lnm]])
  ## --------------------------------
  if (length(original)==0 | length(replication)==0)
    stop("!inference! arguments 'original' and 'replication' must be given")
  ## ---
  ltlev2 <- testlevel/2
  lrlvth <-
    i.def(rlv.threshold[1],
          c(getOption("rlv.threshold")[1],
            relevance.options[["rlv.threshold"]][1]))[1]
  ## ---
  lo <- inference(original)
  lr <- inference(replication)
##-   lor <- lo[,c("estimate","se","scatter","Rle","Rlp","Rls")] *
##-     lr[,"Rle"]/lr[,"estimate"] / (lo[,"Rle"]/lo[,"estimate"])
  lnr <- lr$n
  lno <- lo$n
  lest <- (lr$estimate-lo$estimate)/2
  names(lest) <- row.names(replication)
  lse <- sqrt(lo$se^2+lr$se^2)/2
  ltst <- lest/lse
  lpv <- pnorm(ltst)
  lq <- -qnorm(testlevel/2) ## qt(testlevel/2, min(lno,lnr)-1)
  lsig <- ltst/lq
  lciw <- lq*lse
  lci <- lest+outer(lciw, c(-1,1))
  lsc <- sqrt((lo$scatter^2*(lno-1)+lr$scatter^2*(lnr-1))/(lno+lnr-2))
  leff <- lest/lsc
  leffci <- lci/lsc
  lrlvci <- leffci/lrlvth
  lrlvclass <- rlvClass(-leff, lciw/lsc, rlv.threshold[2])
  ## -----
  lestor <- list(original=lo, replication=lr)
  lcl <- rplClass(lrlvclass, lr$rlvclass, lr$Rle)
  ## ---
  rr <-
    data.frame(estimate=lest, se=lse, teststatstic=ltst,
               p.value=lpv, Sig0=lsig, ciLow=lci[,1], ciUp=lci[,2],
               scatter=lsc,     
               effect=leff, effLow=leffci[,1], effUp=leffci[,2],
               Rle=leff/lrlvth, Rls=lrlvci[,1], Rlp=lrlvci[,2],
               rplclass=lcl,
               row.names=row.names(original))
  if (nrow(rr)==1) rr <- rr[1,]
  structure( rr,
    class = c("inference", "replication", "data.frame"),
    type="simple", method = "replication", effectname = "effect difference",
    n = c(lno, lnr),
    estimate = structure(lestor, class="inference"), 
    df = c(lo$df, lr$df),
    rplclass = lcl, 
    rlv.threshold = lrlvth,
    rlv.type = "standardized"
  )
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
    c("Cnf","CnfW","Att","Enh","Amb","Anh","Ctr")[
      match(rlvclassr, c("Rlv","Amb.Sig","Rlv","Rlv","Amb","Ngl","Ctr"))]
  rr <- ifelse(rr=="Cnf",
        ifelse(rlvclassd=="Rlv", "Att",
        ifelse(rlvclassd=="Ctr", "Enh", rr)), rr)
  rr <- ifelse(rr=="CnfW",
        ifelse(rlvclassd=="Rlv", "Att", 
        ifelse(rler<1, "Amb", rr)), rr)
  rr
}

## -------------------------------------------------------------           
## fisherz <- function(r) 0.5*log((1+r)/(1-r))
## ===============================================================================
