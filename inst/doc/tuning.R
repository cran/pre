## ---- message=FALSE, warning=FALSE--------------------------------------------
library("caret")
library("pre")
library("pROC")
library("ggplot2")

## -----------------------------------------------------------------------------
BigSummary <- function (data, lev = NULL, model = NULL) {
  brscore <- try(mean((data[, lev[2]] - ifelse(data$obs == lev[2], 1, 0)) ^ 2),
                 silent = TRUE)
  rocObject <- try(pROC::roc(ifelse(data$obs == lev[2], 1, 0), data[, lev[2]],
                             direction = "<", quiet = TRUE), silent = TRUE)
  if (inherits(brscore, "try-error")) brscore <- NA
  rocAUC <- if (inherits(rocObject, "try-error")) {
    NA
  } else {
    rocObject$auc
  }
  return(c(AUCROC = rocAUC, Brier = brscore))
}

## -----------------------------------------------------------------------------
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1,
                           classProbs = TRUE, ## get probabilities, not class labels
                           summaryFunction = BigSummary, verboseIter = TRUE)

## -----------------------------------------------------------------------------
preGrid <- getModelInfo("pre")[[1]]$grid(
  maxdepth = 3L:4L,
  learnrate = c(.01, .05, .1),
  penalty.par.val = c("lambda.1se", "lambda.min"),
  sampfrac = c(0.5, 0.75, 1.0))
head(preGrid)

## -----------------------------------------------------------------------------
carrillo$sexo <- factor(paste0("g", as.character(carrillo$sexo)))

## -----------------------------------------------------------------------------
set.seed(42) 
train_ids <- sample(1:nrow(carrillo), .75*nrow(carrillo))

## ---- eval=FALSE--------------------------------------------------------------
#  set.seed(42)
#  pre_tune <- train(sexo ~ ., data = carrillo[train_ids, ], method = "pre",
#                    ntrees = 500, family = "binomial",
#                    trControl = fitControl, tuneGrid = preGrid,
#                    metric = "Brier", ## Specify "AUCROC" for optimizing AUC
#                    maximize = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  set.seed(42)
#  pre_tune2 <- train(sexo ~ ., data = carrillo[train_ids, ], method = "pre",
#                    ntrees = 1000, family = "binomial",
#                    trControl = fitControl, tuneGrid = preGrid,
#                    metric = "Brier", maximize = TRUE)

## ---- echo=FALSE, eval=FALSE--------------------------------------------------
#  save(pre_tune, pre_tune2, file = "Tuning_results.Rda")

## ---- echo=FALSE--------------------------------------------------------------
load("Tuning_results.Rda")

## ---- fig.width=5, fig.height=4-----------------------------------------------
ids <- which(pre_tune$results$Brier == min(pre_tune$results$Brier))
pre_tune$results[ids, c(1:6, 10)]
plot(pre_tune,
     xlab = list(cex = .7), ylab = list(cex = .7),
     scales = list(cex=.7),
     par.strip.text=list(cex=.7))

## ---- fig.width=5, fig.height=4-----------------------------------------------
ids2 <- which(pre_tune2$results$Brier == min(pre_tune2$results$Brier))
pre_tune2$results[ids2, c(1:6, 10)]
plot(pre_tune2, 
     xlab = list(cex = .7), ylab = list(cex = .7),
     scales = list(cex=.7),
     par.strip.text=list(cex=.7))

## -----------------------------------------------------------------------------
set.seed(42)
opt_pre_mod <- pre(formula = sexo ~ ., data = carrillo[train_ids, ],
                   sampfrac = 1, maxdepth = 4, ntrees = 1000, family = "binomial")

## -----------------------------------------------------------------------------
set.seed(42)
def_pre_mod <- pre(formula = sexo ~ ., data = carrillo[train_ids, ],
                   family = "binomial")

## ---- fig.width=5, fig.height=3-----------------------------------------------
print(opt_pre_mod, penalty.par.val = "lambda.1se")
importance(opt_pre_mod, penalty.par.val = "lambda.1se", cex = .7,
           cex.main = .7, cex.lab = .7)

## ---- warning=FALSE, message=FALSE--------------------------------------------
pre_preds_opt <- predict(opt_pre_mod, newdata = carrillo[-train_ids, ], 
                         type = "response", penalty.par.val = "lambda.1se")
y_test <- as.numeric(carrillo[-train_ids, "sexo"])-1
mean((pre_preds_opt - y_test)^2) ## Brier score
sd((pre_preds_opt - y_test)^2)/sqrt(length(y_test)) ## standard error of SEL
auc(response = carrillo[-train_ids, "sexo"], predictor = pre_preds_opt)

## ---- warning=FALSE, message=FALSE--------------------------------------------
summary(def_pre_mod)
pre_preds_def <- predict(def_pre_mod, newdata = carrillo[-train_ids, ], 
                         type = "response")
mean((pre_preds_def - y_test)^2) ## Brier score
sd((pre_preds_def - y_test)^2)/sqrt(length(y_test)) ## standard error of SEL 
auc(response = carrillo[-train_ids, "sexo"], predictor = pre_preds_def)

