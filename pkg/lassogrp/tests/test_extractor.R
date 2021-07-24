library(lassogrp)

set.seed(719)

n <- 100
p <- 10

x <- matrix(runif(n * p, min = -2.5, max = 2.5), nrow = n, ncol = p)
y <- 4 * sin(x[,1]) + x[,2]^2 + rnorm(n)

#####################
##                 ##
## lassogrp object ##
##                 ##
#####################

lambdas <- c(400, 200, 100, 50, 25, 10, 5)
ind <- c(NA, rep(1:5, each=2))
ctrl <- lassoControl() # trace=0 ..
fit <- lasso(cbind(1,x), y, index = ind, lambda = lambdas,
             standardize = FALSE, model = LinReg(), control = ctrl)

if(FALSE) # FIXME: this fails; lasso.formula is less smart than lm() ! __FIXME__
fitF <- lasso(y ~ x, index = ind, lambda = lambdas,
             standardize = FALSE, model = LinReg(), control = ctrl)

plot(fit, log = "x")

## See whether sub-setting is working correctly
## Can't work with all.equal here because of attributes
if(any(range(coef(fit)[,3:4] - coef(fit[3:4])) != 0))
  stop("Subsetting not working correctly")

## Check whether errors are produced for non-valid examples
tools::assertError(fit[1:12], verbose=TRUE)

## Try to remove everything
## now allowed
(empty.fit <- fit[-(1:8)]) # print(<empty>) now works
stopifnot(exprs = {
    inherits(empty.fit, "lassogrp")
    length(empty.fit$lambda) == 0
})

