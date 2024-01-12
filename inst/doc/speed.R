## -----------------------------------------------------------------------------
airq <- airquality[complete.cases(airquality), ]
airq$Month <- factor(airq$Month)
library("pre")
set.seed(42)
system.time(airq.ens <- pre(Ozone ~ ., data = airq))
summary(airq.ens)

## -----------------------------------------------------------------------------
set.seed(42)
system.time(airq.ens.cart <- pre(Ozone ~ ., data = airq, tree.unbiased = FALSE))
summary(airq.ens.cart)

## ----eval=FALSE, echo=FALSE---------------------------------------------------
#  par(mfrow = c(1, 2))
#  imps <- importance(airq.ens, cex.main = .7, main = "Variable importances (ctree)")
#  imps$varimps
#  imps <- importance(airq.ens.cart, cex.main = .7, main = "Variable importances (CART)")
#  imps$varimps
#  sort(sapply(airq, \(x) length(unique(x))), decr = TRUE)
#  ## number of possible cutpoints for Month is 5!

## -----------------------------------------------------------------------------
set.seed(42)
system.time(airq.ens.md <- pre(Ozone ~ ., data = airq, maxdepth = 1L))
summary(airq.ens.md)

## -----------------------------------------------------------------------------
set.seed(42)
system.time(airq.ens.nt <- pre(Ozone ~ ., data = airq, ntrees = 100L, learnrate = .05))
summary(airq.ens.nt)

## -----------------------------------------------------------------------------
set.seed(42)
system.time(airq.ens.nf <- pre(Ozone ~ ., data = airq, nfolds = 5L))
summary(airq.ens.nf)

