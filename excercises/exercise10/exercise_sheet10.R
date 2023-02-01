#' ### Data Import
library(ggplot2)
library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(mlbench)
library(rpart.plot)
library(mlr3filters)
library(mlr3tuning)
library(kknn)
#' ### Exercise 2
#' 
#' #### b)
#'  
#'  In mlr3 (using the mlr3tuning library), define an appropriate search space to tune over. We want to explore
#'  a range between 1 and 100 for k and the kernel to be chosen from "rectangular", "epanechnikov", "gaussian",
#'  "optimal".
search_space <- ps(
  k = p_int(1, 100),
  scale = p_lgl(),
  kernel = p_fct(c("rectangular", "epanechnikov", "gaussian", "optimal"))
)

#' #### c)
#' 
#' Define task
task <- tsk("german_credit")

#' Test-train-split
set.seed(123)
train_index <- sample(seq_len(task$nrow), (task$nrow - 200), replace = FALSE)
test_index <- setdiff(seq_len(task$nrow), train_index)

#' Setup train & test tasks
task_train <- task$clone()$filter(train_index)
task_test <- task$clone()$filter(test_index)

#' Define learner
lrn_knn <- lrn("classif.kknn")

#' Define different resampling startigies
resampling = rsmp("cv", folds = 5)

#' Create combined terminator object
terminator_evals <- trm("evals", n_evals = 200)
terminator_runtime <- trm("run_time", secs = 30)
terminator <- trm(
  "combo",
  list(terminator_evals, terminator_runtime), any = TRUE
)

#' Create tuning instance from the above components
instance <- TuningInstanceSingleCrit$new(
  task = task_train,
  learner = lrn_knn,
  resampling = resampling,
  terminator = terminator,
  search_space = search_space
)

#' Create random-search optimizer
optimizer <- tnr("random_search", batch_size = 20)

#' Tune!
set.seed(123)
optimizer$optimize(instance)

#' #### c)
#' 
#' Tune w.r.t. AUC
lrn_knn <- lrn("classif.kknn", predict_type = "prob")
instance_auc <- TuningInstanceSingleCrit$new(
  task = task_train,
  learner = lrn_knn,
  resampling = resampling,
  terminator = terminator,
  search_space = search_space,
  measure = msr("classif.auc")
)
set.seed(123)
optimizer$optimize(instance_auc)

#' #### d)
#' 
#' Visualize with suiting command 
autoplot(instance_auc)

#' #### e)
#' 
#' Rerun with log transformed k
search_space = ps(
  "k" = p_dbl(log(1), log(100)),
  scale = p_lgl(),
  kernel = p_fct(c("rectangular", "epanechnikov", "gaussian", "optimal")),
  .extra_trafo = function(x, param_set) {
    x$k = round(exp(x$k))
    return(x)
  })
instance_auc <- TuningInstanceSingleCrit$new(
  task = task_train,
  learner = lrn_knn,
  resampling = resampling,
  terminator = terminator,
  search_space = search_space,
  measure = msr("classif.auc"))
set.seed(123)
optimizer$optimize(instance_auc)


#' #### f)
#' 
#' Visualize with suiting command 
autoplot(instance_auc)

#' #### g)
#' 
#' Train model on found hyperparameter config and calculate AUC
optimal_config <- instance_auc$result_learner_param_vals
lrn_knn$param_set$values <- optimal_config
lrn_knn$train(task_train)
prediction <- lrn_knn$predict(task_test)
prediction$score(msr("classif.auc"))
