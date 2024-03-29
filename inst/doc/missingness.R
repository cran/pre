## ----results='hide', message=FALSE, warning=FALSE, fig.width=5----------------
head(airquality)
nrow(airquality)
library("mice")
md.pattern(airquality, rotate.names = TRUE)

## -----------------------------------------------------------------------------
library("pre")
set.seed(43)
airq.ens <- pre(Wind ~., data = airquality)
airq.ens

## -----------------------------------------------------------------------------
imp0 <- airquality
imp0$Solar.R[is.na(imp0$Solar.R)] <- mean(imp0$Solar.R, na.rm=TRUE)
imp0$Ozone[is.na(imp0$Ozone)] <- mean(imp0$Ozone, na.rm=TRUE)
set.seed(43)
airq.ens.imp0 <- pre(Wind ~., data = imp0)
airq.ens.imp0

## ----message=FALSE, warning=FALSE, fig.width=7--------------------------------
set.seed(42)
imp <- mice(airquality, m = 5, printFlag = FALSE)

## -----------------------------------------------------------------------------
imp1 <- complete(imp, action = "all", include = FALSE)

## -----------------------------------------------------------------------------
set.seed(42)
airq.ens.sta <- mi_pre(Wind ~ . , data = imp1)

## ----fig.width=4, fig.height=7------------------------------------------------
summary(airq.ens.sta)
coefs <- coef(airq.ens.sta)
coefs[coefs$coefficient != 0, ]

## -----------------------------------------------------------------------------
newdata <- mi_mean(imp1)
singleplot(airq.ens.sta, newdata = newdata, varname = "Ozone")
pairplot(airq.ens.sta, newdata = newdata, varnames = c("Ozone", "Solar.R"))

## -----------------------------------------------------------------------------
pre.agg <- function(datasets, ...) {
  result <- list()
  for (i in 1:length(datasets)) {
    result[[i]] <- pre(datasets[[i]], ...)
  }
  result
}

## -----------------------------------------------------------------------------
set.seed(43)
airq.agg <- pre.agg(imp1, formula = Wind ~ .)

## ----results ='hide'----------------------------------------------------------
print.agg <- function(object, ...) {
  result <- list()
  sink("NULL")
  for (i in 1:length(object)) {
    result[[i]] <- print(object[[i]], ...)
  }
  sink()
  print(result)
}
print.agg(airq.agg) ## results suppressed for space considerations

summary.agg <- function(object, ...) {
  for (i in 1:length(object)) summary(object[[i]], ...)
}
summary.agg(airq.agg) ## results suppressed for space considerations

## -----------------------------------------------------------------------------
predict.agg <- function(object, newdata, ...) {
  rowMeans(sapply(object, predict, newdata = newdata, ...))
}
agg_preds <- predict.agg(airq.agg, newdata = airquality[1:4, ])
agg_preds

## -----------------------------------------------------------------------------
coef.agg <- function(object, ...) {
  coefs <- coef(object[[1]], ...)
  coefs <- coefs[coefs$coefficient != 0, ]
  for (i in 2:length(object)) {
    coefs_tmp <- coef(object[[i]], ...)
    coefs <- rbind(coefs, coefs_tmp[coefs_tmp$coefficient != 0, ])
  }
  ## Divide coefficients by the number of datasets:
  coefs$coefficient <- coefs$coefficient / length(object)
  ## Identify identical rules:
  duplicates <- which(duplicated(coefs$description))
  for (i in duplicates) {
    first_match <- which(coefs$description == coefs$description[i])[1]
    ## Add the coefficients:
    coefs$coefficient[first_match] <- 
      coefs$coefficient[first_match] + coefs$coefficient[i]
  }
  ## Remove duplicates:
  if (length(duplicates) > 0) coefs <- coefs[-duplicates, ]
  ## Check if there are- duplicate linear terms left and repeat:
  duplicates <- which(duplicated(coefs$rule))
  for (i in duplicates) {
    first_match <- which(coefs$rule == coefs$rule[i])[1]
    coefs$coefficient[first_match] <- 
      coefs$coefficient[first_match] + coefs$coefficient[i]
  }
  if (length(duplicates) > 0) coefs <- coefs[-duplicates, ]
  ## Return results:
  coefs
}
coef.agg(airq.agg)

## ----eval=FALSE, results='hide', message = FALSE, warning = FALSE-------------
#  k <- 10
#  set.seed(43)
#  fold_ids <- sample(1:k, size = nrow(airquality), replace = TRUE)
#  
#  observed <- c()
#  for (i in 1:k) {
#    ## Separate training and test data
#    test <- airquality[fold_ids == i, ]
#    test <- test[!is.na(test$Ozone), ]
#    test <- test[!is.na(test$Solar.R), ]
#    observed <- c(observed, test$Wind)
#  }
#  
#  preds <- data.frame(observed)
#  preds$LWD <- preds$SI <- preds$MI <- preds$observed
#  nterms <- matrix(nrow = k, ncol = 3)
#  colnames(nterms) <- c("LWD", "SI", "MI")
#  row <- 1
#  
#  for (i in 1:k) {
#  
#    if (i > 1) row <- row + nrow(test)
#  
#    ## Separate training and test data
#    train <- airquality[fold_ids != i, ]
#    test <- airquality[fold_ids == i, ]
#    test <- test[!is.na(test$Ozone), ]
#    test <- test[!is.na(test$Solar.R), ]
#  
#    ## Fit and evaluate listwise deletion
#    premod <- pre(Wind ~ ., data = train)
#    preds$LWD[row:(row+nrow(test)-1)] <- predict(premod, newdata = test)
#    tmp <- print(premod)
#    nterms[i, "LWD"] <- nrow(tmp) - 1
#  
#    ## Fit and evaluate single imputation
#    imp0 <- train
#    imp0$Solar.R[is.na(imp0$Solar.R)] <- mean(imp0$Solar.R, na.rm=TRUE)
#    imp0$Ozone[is.na(imp0$Ozone)] <- mean(imp0$Ozone, na.rm=TRUE)
#    premod.imp0 <- pre(Wind ~., data = imp0)
#    imp1 <- test
#    imp1$Solar.R[is.na(imp1$Solar.R)] <- mean(imp0$Solar.R, na.rm=TRUE)
#    imp1$Ozone[is.na(imp1$Ozone)] <- mean(imp0$Ozone, na.rm=TRUE)
#    preds$SI[row:(row+nrow(test)-1)] <- predict(premod.imp0, newdata = imp1)
#    tmp <- print(premod.imp0)
#    nterms[i, "SI"] <- nrow(tmp) - 1
#  
#    ## Perform multiple imputation
#    imp <- mice(train, m = 5)
#    imp1 <- complete(imp, action = "all", include = FALSE)
#    airq.agg <- pre.agg(imp1, formula = Wind ~ .)
#    preds$MI[row:(row+nrow(test)-1)] <- predict.agg(airq.agg, newdata = test)
#    nterms[i, "MI"] <- nrow(coef.agg(airq.agg)) - 1
#  
#  }

## ----echo=FALSE, eval=FALSE---------------------------------------------------
#  save(preds, nterms, file = "Missing_data_results.Rda")

## ----echo=FALSE---------------------------------------------------------------
load("Missing_data_results.Rda")

## ----fig.width=5, fig.height=5------------------------------------------------
sapply(preds, function(x) mean((preds$observed - x)^2)) ## MSE
sapply(preds, function(x) sd((preds$observed - x)^2)/sqrt(nrow(preds))) ## SE of MSE
var(preds$observed) ## benchmark: Predict mean for all

## ----fig.width=4, fig.height=4------------------------------------------------
boxplot(nterms, main = "Number of selected terms \nper missing-data method",
        cex.main = .8)

## -----------------------------------------------------------------------------
sessionInfo()

## ----echo = FALSE-------------------------------------------------------------
# Austin et al. (2019) refer to Wood et al. (2008) for stacking with multiply imputed data:
# 
# "Stacked Imputed Datasets With Weighted Regressions (W1, W2, and W3)
# 
# This approach entails stacking the M imputed datasets into 1 large dataset and then conducting variable selection in this single stacked dataset. To account for the multiple observations for each subject, weights are incorporated into the regression model when conducting variable selection. Wood et al proposed 3 different sets of weights that could be used: W1: w=1/M, in which each subject is weighted using the reciprocal of the number of imputed datasets; W2: w=(1−f)/M, where f denotes the proportion of missing data across all variables; W3: wj=(1−fj)/M, where fj denotes the proportion of missing data for variable Xj. Using the third approach, a different set of weights is used when assessing the statistical significance of a given candidate predictor variable."
# 
# Austin, P. C., Lee, D. S., Ko, D. T., & White, I. R. (2019). Effect of variable selection strategy on the performance of prognostic models when using multiple imputation. Circulation: Cardiovascular Quality and Outcomes, 12(11), e005927.
# 
# Wood, A. M., White, I. R., & Royston, P. (2008). How should variable selection be performed with multiply imputed data?. Statistics in medicine, 27(17), 3227-3246.

## Does glmnet allow for such an approach? Seems not, as weights are scaled automatically, see also https://stats.stackexchange.com/a/196615/173546

