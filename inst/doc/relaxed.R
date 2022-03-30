## -----------------------------------------------------------------------------
library("pre")

## -----------------------------------------------------------------------------
airq <- airquality[complete.cases(airquality), ]
set.seed(42)
airq.ens <- pre(Ozone ~ ., data = airq)
airq.ens

## ----echo=FALSE, results = "hide"---------------------------------------------
tmp <- print(airq.ens)

## ---- results = "hide", echo=FALSE--------------------------------------------
tab <- airq.ens$glmnet.fit$glmnet.fit
tab <- print(tab)

## ---- echo=FALSE, fig.width=5, fig.height=3.5---------------------------------
plot(airq.ens$glmnet.fit)

## ---- eval=FALSE--------------------------------------------------------------
#  plot(airq.ens$glmnet.fit)
#  tab <- airq.ens$glmnet.fit$glmnet.fit
#  tab <- print(tab)

## -----------------------------------------------------------------------------
tab[9:16, ]

## -----------------------------------------------------------------------------
set.seed(42)
airq.ens.rel <- pre(Ozone ~ ., data = airq, relax = TRUE)

## ---- fig.width=6, fig.height=3.5---------------------------------------------
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

## ---- eval=FALSE--------------------------------------------------------------
#  airq.ens.rel$glmnet.fit$glmnet.fit

## ---- fig.width=5, fig.height=3.5---------------------------------------------
coefs <- coef(airq.ens.rel, gamma = 1, penalty = 8)
coefs[coefs$coefficient != 0, ]
coefs <- coef(airq.ens.rel, gamma = 0, penalty = 8)
coefs[coefs$coefficient != 0, ]

## -----------------------------------------------------------------------------
sessionInfo()

