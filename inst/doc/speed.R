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

## -----------------------------------------------------------------------------
set.seed(42)
system.time(airq.ens.rf <- pre(Ozone ~ ., data = airq, randomForest = TRUE))
summary(airq.ens.rf)

## ----echo=FALSE, eval=FALSE---------------------------------------------------
#  sort(sapply(airq, \(x) length(unique(x))), decr = TRUE)
#  par(mfrow = c(1, 3))
#  importance(airq.ens, cex.lab = .7, cex.axis = .7, cex.main = .7,
#             main = "Variable importances (ctree)")
#  importance(airq.ens.cart, cex.lab = .7, cex.axis = .7, cex.main = .7,
#             main = "Variable importances (CART)")
#  imps.rf <- importance(airq.ens.rf, cex.lab = .7, cex.axis = .7, cex.main = .7,
#                        main = "Variable importances (randomForest)")

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

