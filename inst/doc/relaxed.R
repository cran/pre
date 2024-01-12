## -----------------------------------------------------------------------------
library("pre")

## -----------------------------------------------------------------------------
airq <- airquality[complete.cases(airquality), ]
set.seed(42)
airq.ens.rel <- pre(Ozone ~ ., data = airq, relax = TRUE)

## ----echo=FALSE---------------------------------------------------------------
## Overwrite glmnet plotting function, because secondary x axis and legend do
## not use cex, cex.axis or cex.lab arguments
plot.cv.relaxed <- function (x, se.bands = TRUE, cex = .7, cex.axis= .7, 
                             cex.lab = .7, cex.main = .85,...) 
{
    xr = x$relaxed
    oldpar = par(mar = c(4, 4, 3, 4))
    on.exit(par(oldpar))
    statlist = xr$statlist
    gamma = xr$gamma
    ngamma = length(gamma)
    ylim = range(unlist(lapply(statlist, "[[", "cvm")))
    if (se.bands) {
        cvup = lapply(statlist, "[[", "cvup")
        cvlo = lapply(statlist, "[[", "cvlo")
        ylim = range(ylim, unlist(cvup), unlist(cvlo))
    }
    xlim = log(range(unlist(lapply(statlist, "[[", "lambda"))) + 
        1e-05)
    cvcolors = rainbow(ngamma, start = 0.1, end = 1)
    with(statlist[[ngamma]], plot(log(lambda), cvm, type = "n", 
        xlab = expression(Log(lambda)), ylab = x$name, ylim = ylim, 
        xlim = xlim, cex = cex, cex.axis= cex.axis, cex.lab = cex.lab,
        cex.main = cex.main))
    if (se.bands) {
        for (i in seq(ngamma)) with(statlist[[i]], polygon(c(log(lambda), 
            rev(log(lambda))), c(cvup, rev(cvlo)), col = "floralwhite", 
            border = "antiquewhite"))
    }
    for (i in seq(ngamma)) with(statlist[[i]], lines(log(lambda), 
        cvm, lwd = 1, col = cvcolors[i]))
    mins = log(c(xr$lambda.min, xr$lambda.1se))
    abline(v = mins, lty = 3)
    dof = statlist[[1]]$nzero
    lambda = statlist[[1]]$lambda
    axis(side = 3, at = log(lambda), labels = paste(dof), tick = FALSE, 
        line = 0, cex = cex, cex.axis= cex.axis, cex.lab = cex.lab)
    shape::colorlegend(posy = c(0.2, 0.8), posx = c(0.93, 0.945) - 0.03, 
        col = rainbow(ngamma, start = 0.1, end = 1), zlim = c(0, 
            1), zval = gamma, main = expression(gamma), digit = 2, cex=cex)
    invisible()
}

## ----fig.width=5, fig.height=3------------------------------------------------
plot(airq.ens.rel$glmnet.fit)

## -----------------------------------------------------------------------------
fit <- airq.ens.rel$glmnet.fit$relaxed
mat <- data.frame(lambda.1se = c(fit$lambda.1se, fit$gamma.1se, fit$nzero.1se),
                  lambda.min = c(fit$lambda.min, fit$gamma.min, fit$nzero.min),
                  row.names = c("lamda", "gamma", "# of non-zero terms"))
mat

## -----------------------------------------------------------------------------
summary(airq.ens.rel)
summary(airq.ens.rel, penalty = "lambda.min")
summary(airq.ens.rel, penalty = 8, gamma = 0)
summary(airq.ens.rel, penalty = 8, gamma = 1)

## ----eval=FALSE---------------------------------------------------------------
#  airq.ens.rel$glmnet.fit$glmnet.fit

## ----fig.width=5, fig.height=3.5----------------------------------------------
opt_pars <- prune_pre(airq.ens.rel, nonzero = 5)

## -----------------------------------------------------------------------------
set.seed(42)
airq.ens.ad <- pre(Ozone ~ ., data = airq, ad.alpha = 0)
summary(airq.ens.ad)

## -----------------------------------------------------------------------------
set.seed(42)
airq.ens.rel.ad <- pre(Ozone ~ ., data = airq, relax = TRUE, ad.alpha = 0)
print(airq.ens.rel.ad)

## ----echo=FALSE---------------------------------------------------------------
sessionInfo()

