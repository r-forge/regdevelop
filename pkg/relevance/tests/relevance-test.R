## install.packages("~/R/regdevelop/pkg/relevance_1.3.tar.gz", repos=NULL, lib="~/local/R_libs")
require(relevance)

##
d.permeability <-
  data.frame(perm = c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46,
                      1.15, 0.88, 0.90, 0.74, 1.21), atterm = rep(1:0, c(10,5))
             )
rtt <-t.test(perm~atterm, data=d.permeability, var.equal=FALSE)
rr <-twosamples(perm~atterm, data=d.permeability, var.equal=FALSE)
stopifnot(all(abs(rtt$conf.int+rr[c("ciUp","ciLow")])<1e-10))
stopifnot(abs( rtt$p.value-rr["p.value"])<1e-10)

rr1 <- twosamples(rep(0:1,c(5,20)))
onesample(rep(0:1,c(5,20)))
rr2 <- twosamples(rep(0:1,c(5,20)), rep(c(0:1,0:1),c(2,3,12,8)), rlv.threshold=0.1)
(rrf <- fisher.test(rep(0:1,c(5,20)), rep(c(0:1,0:1),c(2,3,12,8))))

## one sample
data(sleep)
dd <- subset(sleep, group==2)
rr <- onesample(60*dd$extra, rlv.threshold=60, standardize=FALSE)
onesample(I(60*extra) ~ 1, data=sleep, subset=group==2,
          rlv.threshold=60, standardize=FALSE)
twosamples(I(60*extra) ~ group, data=sleep, rlv.threshold=60, standardize=FALSE)

topt <- options(contrasts=c("contr.sum", "contr.poly"))
rr <- lm(I(60*extra) ~ group, data=sleep)
rte <- termeffects(rr)
plot(termeffects(rr))
options(topt) ## restore options
## ------------------------------------------------------------
ff <- function(dd, ...) twosamples(perm~atterm, data=dd, ...)
ff(d.permeability)
ff <- function(x, ...) twosamples(x, ...)
dd <- subset(sleep, group==2)
ff(dd$extra)
twosamples(dd$extra)

## ==================================================================

data(d.blast)
rlm <- lm(log10(tremor)~location+log10(distance)+log10(charge), data=d.blast)
rt <- termtable(rlm)
rt
inference(rlm)
rte <- termeffects(rlm)
print(rte, show.inference=c("classical","coefRls","coefRls.symbol"), single=TRUE)
plot(rte, single=TRUE)

rr <- inference(rlm)
rlm <- lm(log10(tremor)~location+log10(distance)+log10(charge),
         data=d.blast, subset=location %in% c("loc2", "loc4", "loc6"))

rpr <- print(termeffects(rlm), print=FALSE)
attr(rpr, "head") <- sub("lm", "Linear Regression", attr(rpr, "head"))
rpr

data(swiss)
rr <- lm(Fertility ~ . , data = swiss)
rt <- termtable(rr)
rt
rtp <- print(rt)
plot(rt)
## ----
## glm
## Dobson (1990) Page 93: Randomized Controlled Trial :
d.dobson <- data.frame(treatment=gl(3,3), outcome=gl(3,1,9),
                       counts=c(18,17,15,20,10,20,25,13,12)) 
rglm <- glm(counts ~ outcome + treatment, data=d.dobson, family = poisson())
summary(rglm)
rt <- termtable(rglm)
## relevance:::print.inference(rt, show="test")
print(rt, show="test")
(rte <- termeffects(rglm))
rr <- inference(rglm)
## an example with offsets from Venables & Ripley (2002, p.189)
data(anorexia, package = "MASS")
rglma <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
             family = gaussian, data = anorexia)
summary(rglma)
termtable(rglma)
rte <- termeffects(rglma)
     
## A Gamma example, from McCullagh & Nelder (1989, pp. 300-2)
clotting <- data.frame(
  u = c(5,10,15,20,30,40,60,80,100),
  lot1 = c(118,58,42,35,27,25,21,19,18),
  lot2 = c(69,35,26,21,18,16,13,12,12))
rglmc1 <- glm(lot1 ~ log(u), data = clotting, family = Gamma)
rglmc2 <- glm(lot2 ~ log(u), data = clotting, family = Gamma)

rt <- termtable(rglmc1)
## Aliased ("S"ingular) -> 1 NA coefficient
rglmc3 <- glm(lot2 ~ log(u) + log(u^2), data = clotting, family = Gamma)
## does not give the coefficient of log(u)

data(housing, package="MASS")
rpolr <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
rt <- termtable(rpolr)
rt

##- data(d.surveyenvir)
##- rpolr2 <- MASS::polr(disturbance ~ age+education+location, data=d.surveyenvir)
##- rt <- termtable(rpolr2)

## ----------------------
data(ovarian) ## , package="survival"
rsrw <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps + rx,
                         data=ovarian, dist='weibull', scale=1)
termtable(rsrw)
summary(rsrw)

rsre <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps + rx, 
                          data=ovarian, dist="exponential")
termtable(rsre)     

data(tobin, package="survival")
rsrtobin <-
  survival::survreg(survival::Surv(durable, durable>0, type='left') ~ age + quant,
                         data=tobin, dist='gaussian')
rt <- termtable(rsrtobin)
