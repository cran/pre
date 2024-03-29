context("Tests the pre functions with S3 methods and maxdepth sampler.")


test_that("maxdepth.sampler gives previous results", {
  func1 <- maxdepth_sampler()
  set.seed(42)
  maxdepths <- func1(100)
  # save_to_test(maxdepths, "maxdepths_default")
  expect_equal(maxdepths, read_to_test("maxdepths_default"), tolerance = 1.490116e-08)
  
  func2 <- maxdepth_sampler(av.no.term.nodes = 16L)
  set.seed(42)
  maxdepths2 <- func2(100)
  # save_to_test(maxdepths2, "maxdepths_16_term_nodes")
  expect_equal(maxdepths2, read_to_test("maxdepths_16_term_nodes"), tolerance = 1.490116e-08)
  
  func3 <- maxdepth_sampler(av.tree.depth = 7)
  set.seed(42)
  maxdepths3 <- func3(100)
  # save_to_test(maxdepths3, "maxdepths_tree_depth_7")
  expect_equal(maxdepths3, read_to_test("maxdepths_tree_depth_7"), tolerance = 1.490116e-08)
})


test_that("summary.pre gives previous results", {
  set.seed(42)
  airq.ens <- pre(Ozone ~ ., data=airquality, ntrees = 10)
  summar <- capture.output(summary(airq.ens))
  # save_to_test(summar, "pre_summary")
  expect_equal(summar, read_to_test("pre_summary"), tolerance = 1.490116e-08)
})


test_that("gpe_rules_pre gives same results as pre with airquality data", {
  set.seed(42)
  gpe.mod <- gpe(Ozone ~ ., data = airquality,  
                 base_learners = list(gpe_rules_pre(ntrees = 25L, learnrate = .1, tree.unbiased = FALSE)))
  set.seed(42)
  pre.mod <- pre(Ozone ~ ., data = airquality, ntrees = 25L, learnrate = .1, tree.unbiased = FALSE, 
                 type = "rules")
  expect_equal(gpe.mod$glmnet.fit$nzero, pre.mod$glmnet.fit$nzero, tolerance = 1.490116e-08)
  expect_equal(gpe.mod$glmnet.fit$cvm, pre.mod$glmnet.fit$cvm, tolerance = 1.490116e-08)
})


test_that("function explain gives previous results", {
  set.seed(42)
  airq.ens <- pre(Ozone ~ ., data=airquality, ntrees = 10)
  expl <- explain(airq.ens, newdata = airquality)
  # save_to_test(expl, "explain")
  expect_equal(expl, read_to_test("explain"))
})

test_that("Importance gives previous results with airquality data",{
  set.seed(42)
  airq.ens <- pre(Ozone ~ ., data=airquality, ntrees = 10)
  
  imp1 <- importance(airq.ens, plot = FALSE)
  # save_to_test(imp1, "airquality_w_importance1")
  expect_equal(imp1, read_to_test("airquality_w_importance1"), tolerance = 1.490116e-08)
  ## Check whether variable and baselearner importances add up:
  expect_equal(sum(imp1$varimps$imp), sum(imp1$baseimps$imp), tolerance = 1.490116e-08)
  
  imp2 <- importance(airq.ens, global = FALSE, plot = FALSE)
  # save_to_test(imp2, "airquality_w_importance2")
  expect_equal(imp2, read_to_test("airquality_w_importance2"), tolerance = 1.490116e-08)
  ## Check whether variable and baselearner importances add up:
  expect_equal(sum(imp2$varimps$imp), sum(imp2$baseimps$imp), tolerance = 1.490116e-08)
  
  imp3 <- importance(airq.ens, global = FALSE, quantprobs = c(0, .25), plot = FALSE)
  # save_to_test(imp3, "airquality_w_importance3")
  expect_equal(imp3, read_to_test("airquality_w_importance3"), tolerance = 1.490116e-08)
  ## Check whether variable and baselearner importances add up:
  expect_equal(sum(imp3$varimps$imp), sum(imp3$baseimps$imp), tolerance = 1.490116e-08)
})

test_that("Coef gives previous results with airquality data", {
  set.seed(42)
  airq.ens <- pre(Ozone ~ ., data=airquality, ntrees = 10)
  coefs <- coef(airq.ens)
  # save_to_test(coefs, "airquality_w_pre_coef")
  expect_equal(coefs, read_to_test("airquality_w_pre_coef"), tolerance = 1.490116e-08)
})

## TODO: parallel = TRUE and FALSE do not yield same result
test_that("cvpre gives previous results with airquality data", {
  set.seed(42)
  airq.ens <- pre(Ozone ~ ., data=airquality, ntrees = 10)
  
  set.seed(7385056)
  airq.cv <- cvpre(airq.ens, k = 2, print = FALSE, parallel = FALSE)
  
  library("doParallel")
  cl <- makeCluster(2L)
  registerDoParallel(cl)
  set.seed(7385056)
  airq.cv.par <- cvpre(airq.ens, k = 2L, print = FALSE, parallel = TRUE)
  stopCluster(cl)
  
  #expect_equal(airq.cv, airq.cv.par, tolerance = 1.490116e-08)
  # save_to_test(airq.cv, "airquality_w_pre_cv")
  expect_equal(airq.cv, read_to_test("airquality_w_pre_cv"), tolerance = 1.490116e-08)
  # save_to_test(airq.cv.par, "airquality_w_pre_cv_par")
  expect_equal(airq.cv.par, read_to_test("airquality_w_pre_cv_par"), tolerance = 1.490116e-08)
})

test_that("bsnullinteract and interact gives previous results with airquality data", {
  set.seed(42)
  airq.ens <- pre(Ozone ~ ., data=airquality, ntrees = 10)
  
  set.seed(8969591)
  nullmods <- bsnullinteract(airq.ens, nsamp = 1)
  inter <- interact(airq.ens, c("Temp", "Wind"), nullmods = nullmods, plot = FALSE)
  
  for(i in 1:1) {
    nullmods[[i]] <- nullmods[[i]][names(nullmods[[i]]) %in%  c(
      "x_scales", "modmat", "wins_points", "modmat")]
  }
  # save_to_test(nullmods, "airquality_w_bsnullinteract")
  expect_equal(nullmods, read_to_test("airquality_w_bsnullinteract"), tolerance = 1.490116e-08)

  # save_to_test(inter, "airquality_w_inter")
  expect_equal(inter, read_to_test("airquality_w_inter"), tolerance = 1.490116e-08)
})

test_that("Print gives previous results with airquality",{
  old <- getOption("digits")
  on.exit(options(digits = old))
  options(digits = 4)
  set.seed(42)
  airq.ens <- pre(Ozone ~ ., data=airquality, ntrees = 10)
  
  # save_to_test(capture.output(print(airq.ens)), "airquality_print_output")
  expect_equal(capture.output(print(airq.ens)), read_to_test("airquality_print_output"), tolerance = 1.490116e-08)
  
  #####
  # Change digits
  options(digits = 2)
  
  # save_to_test(capture.output(print(airq.ens)), "airquality_print_output_less_digits")
  expect_equal(capture.output(print(airq.ens)), read_to_test("airquality_print_output_less_digits"), tolerance = 1.490116e-08)
})

test_that("Predict gives previous results with airquality data", {
  set.seed(42)
  airq.ens <- pre(Ozone ~ ., data=airquality, ntrees = 10)
  preds <- predict(airq.ens, airquality)

  # save_to_test(preds, "airquality_preds")
  expect_equal(preds, read_to_test("airquality_preds"), tolerance = 1.490116e-08)
  
  #####
  # Should give the same data to use in fit is not passed
  other_preds <- predict(airq.ens)
  
  expect_equal(other_preds, preds)
})

test_that("`delete_duplicates_complements` gives the same regardless of `sparse` argument", {
  set.seed(25106787)
  X <- data.frame(
    matrix(runif(10 * 20), 10, dimnames = list(NULL, paste0("X", 1:20))))
  rules <- paste0(
    "X", sample.int(20, 100, replace = TRUE), 
    ifelse(runif(100) > .5, ">", "<="), runif(100))
  
  confs <- expand.grid(
    removecomplements = c(T, F), removeduplicates = c(T, F), 
    return.dupl.compl = c(T, F))
  
  for(i in 1:nrow(confs)) { 
    v <- confs[i, ]
    eval(bquote(
      expect_equal(
        delete_duplicates_complements(
          removecomplements = .(v$removecomplements), 
          removeduplicates  = .(v$removeduplicates), 
          return.dupl.compl = .(v$return.dupl.compl),
          rules = rules, data = X, sparse = TRUE),
        delete_duplicates_complements(
          removecomplements = .(v$removecomplements), 
          removeduplicates  = .(v$removeduplicates), 
          return.dupl.compl = .(v$return.dupl.compl),
          rules = rules, data = X))))
  }
})

test_that("`.get_most_sparse_rule` gives the spares of rules", {
  dat <- data.frame(x = 1:5)
  rules <- paste("x >", 1:4)
  
  expect_equal(
    .get_most_sparse_rule(rules, dat),
    c(`x > 1` = "!(x > 1)", `x > 2` = "!(x > 2)", `x > 3` = "x > 3", 
      `x > 4` = "x > 4"))
})


test_that("`list.rules` gives previous results", {
  airct <- ctree(Ozone ~ ., data = airquality)
  rules <- list()
  rules[[1]] <- list.rules(airct, removecomplements = FALSE, singleconditions = TRUE)
  rules[[2]] <- list.rules(airct, removecomplements = FALSE, singleconditions = FALSE)
  rules[[3]] <- list.rules(airct, removecomplements = FALSE, singleconditions = "only")
  rules[[4]] <- list.rules(airct, singleconditions = TRUE)
  rules[[5]] <- list.rules(airct, singleconditions = "only")
  rules[[6]] <- list.rules(airct, removecomplements = FALSE)
  
  # save_to_test(rules, "rules")
  expect_equal(rules, read_to_test("rules"))
})


