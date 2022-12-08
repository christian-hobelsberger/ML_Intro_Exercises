#' ### Data Import
library(ggplot2)
library(mlr3)
library(mlr3learners)
library(mlr3viz)
library(mlbench)
#' ### Exercise 2
#' 
#' #### b)
#'  
#' Get data set
(task_credit <- mlr_tasks$get("german_credit"))

#' Create learner
learner_credit = mlr3::lrn("classif.log_reg")

#' Train
learner_credit$train(task_credit)

#' Predict
predictions <- learner_credit$predict(task_credit)

#' Compute CE
predictions$score()

#' #### c)
#' 
#' Define different resampling startigies
resampling_cv_3x10 = rsmp("repeated_cv", folds = 10, repeats = 3)
resampling_cv_10x3 = rsmp("repeated_cv", folds = 3, repeats = 10)
resampling_ho = rsmp("holdout", ratio = 0.9)

#' Create task
(task_credit2 <- mlr_tasks$get("german_credit"))

#' Resample and store results
result_cv_3x10 = resample(task_credit, learner_credit, resampling_cv_3x10, store_models = TRUE)
result_cv_10x3 = resample(task_credit, learner_credit, resampling_cv_10x3, store_models = TRUE)
result_ho = resample(task_credit, learner_credit, resampling_ho, store_models = TRUE)

# Evaluate with stratification
task_stratified <- task_credit$clone()
task_stratified$set_col_roles("foreign_worker", roles = "stratum")
result_stratified <- resample(
  task_stratified, learner_credit, resampling_cv_3x10, store_models = TRUE)

# Aggregate results over splits
print(sapply(
  list(result_cv_3x10, result_cv_10x3, result_stratified, result_ho),
  function(i) i$aggregate()))

# Resample
rr = resample(task_credit2, learner_credit2, cv10, )

